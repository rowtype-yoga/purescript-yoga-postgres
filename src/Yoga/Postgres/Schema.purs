module Yoga.Postgres.Schema where

import Prelude

import Data.Array as Array
import Data.Array (intercalate, mapWithIndex, foldl)
import Data.DateTime (DateTime)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Nullable (toNullable)
import JS.BigInt (BigInt)
import Unsafe.Coerce (unsafeCoerce)
import Data.Reflectable (class Reflectable, reflectType)
import Data.String.Regex (regex, replace') as Regex
import Data.String.Regex (Regex) as Regex
import Data.String.Regex.Flags (global) as Regex
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\))
import Effect.Aff (Aff)
import Foreign (Foreign, unsafeToForeign)
import Prim.Boolean (True, False)
import Prim.Row (class Cons, class Lacks, class Union, class Nub) as Row
import Prim.RowList as RL
import Prim.RowList (class RowToList)
import Prim.Symbol (class Cons, class Append) as Symbol
import Prim.TypeError (class Fail, Beside, Text, Quote)
import Record (get) as Record
import Type.Data.Boolean (class Or)
import Type.Proxy (Proxy(..))
import Type.RowList (class ListToRow)
import Yoga.JSON (class ReadForeign, readImpl)
import Yoga.Postgres as PG

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Core phantom types
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

data Table :: Symbol -> Row Type -> Type
data Table name columns = Table

data Column :: Type -> Type -> Type
data Column typ constraints = Column

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Constraint phantom types
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

data PrimaryKey
data AutoIncrement
data Unique
data None

data Default :: Symbol -> Type
data Default a

data DefaultInt :: Int -> Type
data DefaultInt a

data DefaultBool :: Boolean -> Type
data DefaultBool a

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- JSONB type wrapper
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

newtype Jsonb = Jsonb Foreign

derive instance Newtype Jsonb _

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Nullability: inferred from Maybe
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class IsNullable a where
  isNullable :: Proxy a -> Boolean

instance IsNullable (Maybe a) where
  isNullable _ = true
else instance IsNullable a where
  isNullable _ = false

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- PureScript type → Postgres type name
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class PGTypeName a where
  pgTypeName :: Proxy a -> String

instance PGTypeName Int where
  pgTypeName _ = "INTEGER"

instance PGTypeName String where
  pgTypeName _ = "TEXT"

instance PGTypeName Boolean where
  pgTypeName _ = "BOOLEAN"

instance PGTypeName Number where
  pgTypeName _ = "DOUBLE PRECISION"

instance PGTypeName DateTime where
  pgTypeName _ = "TIMESTAMPTZ"

instance PGTypeName BigInt where
  pgTypeName _ = "BIGINT"

instance PGTypeName Jsonb where
  pgTypeName _ = "JSONB"

instance PGTypeName a => PGTypeName (Array a) where
  pgTypeName _ = pgTypeName (Proxy :: Proxy a) <> "[]"

instance PGTypeName a => PGTypeName (Maybe a) where
  pgTypeName _ = pgTypeName (Proxy :: Proxy a)

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Constraint → DDL fragment
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class RenderConstraint a where
  renderConstraint :: Proxy a -> String

instance RenderConstraint PrimaryKey where
  renderConstraint _ = "PRIMARY KEY"

instance RenderConstraint AutoIncrement where
  renderConstraint _ = "GENERATED ALWAYS AS IDENTITY"

instance RenderConstraint Unique where
  renderConstraint _ = "UNIQUE"

instance RenderConstraint None where
  renderConstraint _ = ""

instance Reflectable sym String => RenderConstraint (Default sym) where
  renderConstraint _ = "DEFAULT " <> reflectType (Proxy :: Proxy sym)

instance Reflectable val Int => RenderConstraint (DefaultInt val) where
  renderConstraint _ = "DEFAULT " <> show (reflectType (Proxy :: Proxy val))

instance Reflectable val Boolean => RenderConstraint (DefaultBool val) where
  renderConstraint _ =
    if reflectType (Proxy :: Proxy val) then "DEFAULT true"
    else "DEFAULT false"

instance (RenderConstraint a, RenderConstraint b) => RenderConstraint (a /\ b) where
  renderConstraint _ = do
    let a = renderConstraint (Proxy :: Proxy a)
    let b = renderConstraint (Proxy :: Proxy b)
    case a, b of
      "", s -> s
      s, "" -> s
      s1, s2 -> s1 <> " " <> s2

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- DDL generation
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class CreateTableDDL a where
  createTableDDL :: String

class RenderColumnsRL :: RL.RowList Type -> Constraint
class RenderColumnsRL rl where
  renderColumnsRL :: Proxy rl -> Array String

instance RenderColumnsRL RL.Nil where
  renderColumnsRL _ = []

instance
  ( IsSymbol name
  , PGTypeName typ
  , IsNullable typ
  , RenderConstraint constraints
  , RenderColumnsRL tail
  ) =>
  RenderColumnsRL (RL.Cons name (Column typ constraints) tail) where
  renderColumnsRL _ = do
    let colName = reflectSymbol (Proxy :: Proxy name)
    let colType = pgTypeName (Proxy :: Proxy typ)
    let nullable = isNullable (Proxy :: Proxy typ)
    let notNull = if nullable then "" else " NOT NULL"
    let constraints = renderConstraint (Proxy :: Proxy constraints)
    let constraintsSuffix = if constraints == "" then "" else " " <> constraints
    [ colName <> " " <> colType <> notNull <> constraintsSuffix ] <> renderColumnsRL (Proxy :: Proxy tail)

instance
  ( IsSymbol name
  , RowToList cols rl
  , RenderColumnsRL rl
  ) =>
  CreateTableDDL (Table name cols) where
  createTableDDL = do
    let tableName = reflectSymbol (Proxy :: Proxy name)
    let columns = renderColumnsRL (Proxy :: Proxy rl)
    "CREATE TABLE " <> tableName <> " (" <> intercalate ", " columns <> ")"

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- INSERT SQL generation
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class IsAutoGenerated a where
  isAutoGenerated :: Proxy a -> Boolean

instance IsAutoGenerated AutoIncrement where
  isAutoGenerated _ = true
else instance IsAutoGenerated (Default a) where
  isAutoGenerated _ = true
else instance IsAutoGenerated (DefaultInt a) where
  isAutoGenerated _ = true
else instance IsAutoGenerated (DefaultBool a) where
  isAutoGenerated _ = true
else instance (IsAutoGenerated a, IsAutoGenerated b) => IsAutoGenerated (a /\ b) where
  isAutoGenerated _ = isAutoGenerated (Proxy :: Proxy a) || isAutoGenerated (Proxy :: Proxy b)
else instance IsAutoGenerated a where
  isAutoGenerated _ = false

class InsertColumnsRL :: RL.RowList Type -> Constraint
class InsertColumnsRL rl where
  insertColumnsRL :: Proxy rl -> Array String

instance InsertColumnsRL RL.Nil where
  insertColumnsRL _ = []

instance
  ( IsSymbol name
  , IsAutoGenerated constraints
  , InsertColumnsRL tail
  ) =>
  InsertColumnsRL (RL.Cons name (Column typ constraints) tail) where
  insertColumnsRL _ =
    let
      rest = insertColumnsRL (Proxy :: Proxy tail)
    in
      if isAutoGenerated (Proxy :: Proxy constraints) then rest
      else [ reflectSymbol (Proxy :: Proxy name) ] <> rest

class InsertSQLFor a where
  insertSQLFor :: String

instance
  ( IsSymbol name
  , RowToList cols rl
  , InsertColumnsRL rl
  ) =>
  InsertSQLFor (Table name cols) where
  insertSQLFor = do
    let tableName = reflectSymbol (Proxy :: Proxy name)
    let cols = insertColumnsRL (Proxy :: Proxy rl)
    let placeholders = cols # mapWithIndex \i _ -> "$" <> show (i + 1)
    "INSERT INTO " <> tableName
      <> " ("
      <> intercalate ", " cols
      <> ")"
      <> " VALUES ("
      <> intercalate ", " placeholders
      <> ")"
      <> " RETURNING *"

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- SELECT SQL generation
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class SelectAllSQLFor a where
  selectAllSQLFor :: String

instance
  ( IsSymbol name
  ) =>
  SelectAllSQLFor (Table name cols) where
  selectAllSQLFor = "SELECT * FROM " <> reflectSymbol (Proxy :: Proxy name)

class WhereClauseRL :: RL.RowList Type -> Constraint
class WhereClauseRL rl where
  whereClauseRL :: Proxy rl -> Int -> Array String

instance WhereClauseRL RL.Nil where
  whereClauseRL _ _ = []

instance (IsSymbol name, WhereClauseRL tail) => WhereClauseRL (RL.Cons name typ tail) where
  whereClauseRL _ idx =
    [ reflectSymbol (Proxy :: Proxy name) <> " = $" <> show idx ]
      <> whereClauseRL (Proxy :: Proxy tail) (idx + 1)

class SelectWhereSQLFor a whereRow where
  selectWhereSQLFor :: String

instance
  ( IsSymbol name
  , RowToList whereRow whereRL
  , WhereClauseRL whereRL
  ) =>
  SelectWhereSQLFor (Table name cols) whereRow where
  selectWhereSQLFor = do
    let tableName = reflectSymbol (Proxy :: Proxy name)
    let conditions = whereClauseRL (Proxy :: Proxy whereRL) 1
    "SELECT * FROM " <> tableName <> " WHERE " <> intercalate " AND " conditions

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- UPDATE SQL generation
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class ColumnCountRL :: RL.RowList Type -> Constraint
class ColumnCountRL rl where
  columnCountRL :: Proxy rl -> Int

instance ColumnCountRL RL.Nil where
  columnCountRL _ = 0

instance ColumnCountRL tail => ColumnCountRL (RL.Cons name typ tail) where
  columnCountRL _ = 1 + columnCountRL (Proxy :: Proxy tail)

class UpdateSQLFor table setRow whereRow where
  updateSQLFor :: String

instance
  ( IsSymbol name
  , RowToList setRow setRL
  , RowToList whereRow whereRL
  , WhereClauseRL setRL
  , WhereClauseRL whereRL
  , ColumnCountRL setRL
  ) =>
  UpdateSQLFor (Table name cols) setRow whereRow where
  updateSQLFor = do
    let tableName = reflectSymbol (Proxy :: Proxy name)
    let setClauses = whereClauseRL (Proxy :: Proxy setRL) 1
    let setCount = columnCountRL (Proxy :: Proxy setRL)
    let whereClauses = whereClauseRL (Proxy :: Proxy whereRL) (setCount + 1)
    "UPDATE " <> tableName
      <> " SET "
      <> intercalate ", " setClauses
      <> " WHERE "
      <> intercalate " AND " whereClauses

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- DELETE SQL generation
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class DeleteSQLFor table whereRow where
  deleteSQLFor :: String

instance
  ( IsSymbol name
  , RowToList whereRow whereRL
  , WhereClauseRL whereRL
  ) =>
  DeleteSQLFor (Table name cols) whereRow where
  deleteSQLFor = do
    let tableName = reflectSymbol (Proxy :: Proxy name)
    let conditions = whereClauseRL (Proxy :: Proxy whereRL) 1
    "DELETE FROM " <> tableName <> " WHERE " <> intercalate " AND " conditions

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Builder-style query API
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- Type-level WHERE clause validation
-- Parses SQL-like syntax, extracts identifiers, validates them as column names
--
-- Recognises:
--   column names  → validated against the table row
--   $N            → parameter placeholders, skipped
--   AND OR NOT IS NULL LIKE IN TRUE FALSE → SQL keywords, skipped
--   = > < >= <= != <> → operators, skipped
--   ( ) , digits  → punctuation, skipped

-- Top-level: walk the Symbol, extract words, validate column-like words
class ValidateWhere :: Symbol -> Row Type -> Constraint
class ValidateWhere sym cols

instance ValidateWhere "" cols
else instance
  ( Symbol.Cons h t sym
  , ValidateWhereGo h t "" cols
  ) =>
  ValidateWhere sym cols

-- Walk character by character, accumulating words
class ValidateWhereGo :: Symbol -> Symbol -> Symbol -> Row Type -> Constraint
class ValidateWhereGo head tail acc cols

-- Space: flush accumulated word, continue
instance
  ( FlushWord acc cols
  , SkipSpaces tail rest
  , ValidateWhere rest cols
  ) =>
  ValidateWhereGo " " tail acc cols

-- Operators and punctuation: flush word, skip char, continue
else instance (FlushWord acc cols, ValidateWhere tail cols) => ValidateWhereGo "=" tail acc cols
else instance (FlushWord acc cols, ValidateWhere tail cols) => ValidateWhereGo ">" tail acc cols
else instance (FlushWord acc cols, ValidateWhere tail cols) => ValidateWhereGo "<" tail acc cols
else instance (FlushWord acc cols, ValidateWhere tail cols) => ValidateWhereGo "!" tail acc cols
else instance (FlushWord acc cols, ValidateWhere tail cols) => ValidateWhereGo "(" tail acc cols
else instance (FlushWord acc cols, ValidateWhere tail cols) => ValidateWhereGo ")" tail acc cols
else instance (FlushWord acc cols, ValidateWhere tail cols) => ValidateWhereGo "," tail acc cols
else instance (FlushWord acc cols, ValidateWhere tail cols) => ValidateWhereGo "'" tail acc cols
else instance (FlushWord acc cols, ValidateWhere tail cols) => ValidateWhereGo "@" tail acc cols
else instance (FlushWord acc cols, ValidateWhere tail cols) => ValidateWhereGo "?" tail acc cols
else instance (FlushWord acc cols, ValidateWhere tail cols) => ValidateWhereGo ":" tail acc cols
else instance (FlushWord acc cols, ValidateWhere tail cols) => ValidateWhereGo "~" tail acc cols
else instance (FlushWord acc cols, ValidateWhere tail cols) => ValidateWhereGo "#" tail acc cols

-- End of string: flush final word
else instance
  ( Symbol.Append acc h acc'
  , FlushWord acc' cols
  ) =>
  ValidateWhereGo h "" acc cols

-- Regular character: accumulate and continue
else instance
  ( Symbol.Append acc h acc'
  , Symbol.Cons nextH nextT tail
  , ValidateWhereGo nextH nextT acc' cols
  ) =>
  ValidateWhereGo h tail acc cols

-- Flush a word: if it looks like a column name, validate it; otherwise skip
class FlushWord :: Symbol -> Row Type -> Constraint
class FlushWord word cols

-- Empty accumulator: nothing to validate
instance FlushWord "" cols

-- $-prefixed: parameter placeholder, skip
else instance FlushWord "$" cols

-- SQL keywords: skip
else instance FlushWord "AND" cols
else instance FlushWord "OR" cols
else instance FlushWord "NOT" cols
else instance FlushWord "IS" cols
else instance FlushWord "NULL" cols
else instance FlushWord "LIKE" cols
else instance FlushWord "ILIKE" cols
else instance FlushWord "IN" cols
else instance FlushWord "TRUE" cols
else instance FlushWord "FALSE" cols
else instance FlushWord "BETWEEN" cols
else instance FlushWord "ANY" cols
else instance FlushWord "ALL" cols

-- Non-keyword: check first character to decide
else instance
  ( Symbol.Cons head rest word
  , FlushWordByHead head word cols
  ) =>
  FlushWord word cols

-- Branch on first character of the word
class FlushWordByHead :: Symbol -> Symbol -> Row Type -> Constraint
class FlushWordByHead head word cols

-- $-prefixed: parameter placeholder, skip
instance FlushWordByHead "$" word cols
-- Digit-prefixed: number literal, skip
else instance FlushWordByHead "0" word cols
else instance FlushWordByHead "1" word cols
else instance FlushWordByHead "2" word cols
else instance FlushWordByHead "3" word cols
else instance FlushWordByHead "4" word cols
else instance FlushWordByHead "5" word cols
else instance FlushWordByHead "6" word cols
else instance FlushWordByHead "7" word cols
else instance FlushWordByHead "8" word cols
else instance FlushWordByHead "9" word cols

-- Anything else: must be a column name
else instance Row.Cons word typ rest cols => FlushWordByHead head word cols

else instance
  Fail (Beside (Beside (Text "Column ") (Quote word)) (Text " does not exist in the table")) =>
  FlushWordByHead head word cols

-- Also validate comma-separated column lists (for select)
class ValidateColumns :: Symbol -> Row Type -> Constraint
class ValidateColumns sym cols

instance ValidateColumns "" cols
else instance
  ( Symbol.Cons h t sym
  , ValidateColumnsGo h t "" cols
  ) =>
  ValidateColumns sym cols

class ValidateColumnsGo :: Symbol -> Symbol -> Symbol -> Row Type -> Constraint
class ValidateColumnsGo head tail acc cols

-- Comma: flush name, continue
instance
  ( FlushWord acc cols
  , SkipSpaces tail rest
  , ValidateColumns rest cols
  ) =>
  ValidateColumnsGo "," tail acc cols

-- Space: flush name, skip to comma or end
else instance
  ( SkipSpaces tail rest
  , ValidateAfterName acc rest cols
  ) =>
  ValidateColumnsGo " " tail acc cols

-- End of string: flush final name
else instance
  ( Symbol.Append acc h acc'
  , FlushWord acc' cols
  ) =>
  ValidateColumnsGo h "" acc cols

-- Regular char: accumulate
else instance
  ( Symbol.Append acc h acc'
  , Symbol.Cons nextH nextT tail
  , ValidateColumnsGo nextH nextT acc' cols
  ) =>
  ValidateColumnsGo h tail acc cols

class ValidateAfterName :: Symbol -> Symbol -> Row Type -> Constraint
class ValidateAfterName acc rest cols

-- End of input
instance FlushWord acc cols => ValidateAfterName acc "" cols

-- Non-empty: branch on first character
else instance
  ( FlushWord acc cols
  , Symbol.Cons h t rest
  , ValidateAfterNameByHead h t cols
  ) =>
  ValidateAfterName acc rest cols

class ValidateAfterNameByHead :: Symbol -> Symbol -> Row Type -> Constraint
class ValidateAfterNameByHead head tail cols

-- Comma: done with this column, continue
instance
  ( SkipSpaces tail rest
  , ValidateColumns rest cols
  ) =>
  ValidateAfterNameByHead "," tail cols

-- Anything else (e.g. "AS ..."): extract the word and handle
else instance
  ( Symbol.Append h t rest
  , ExtractWord rest word afterWord
  , HandleAfterColumnWord word afterWord cols
  ) =>
  ValidateAfterNameByHead h t cols

-- Extract the first word from a Symbol (up to space or end)
class ExtractWord :: Symbol -> Symbol -> Symbol -> Constraint
class ExtractWord sym word rest | sym -> word rest

instance ExtractWord "" "" ""
else instance
  ( Symbol.Cons h t sym
  , ExtractWordGo h t "" word rest
  ) =>
  ExtractWord sym word rest

class ExtractWordGo :: Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Constraint
class ExtractWordGo head tail acc word rest | head tail acc -> word rest

-- Stop on space
instance (SkipSpaces tail rest) => ExtractWordGo " " tail acc acc rest
-- Stop on comma (keep comma in rest)
else instance Symbol.Cons "," tail rest => ExtractWordGo "," tail acc acc rest
-- End of string
else instance Symbol.Append acc h word => ExtractWordGo h "" acc word ""
-- Regular char: accumulate
else instance
  ( Symbol.Append acc h acc'
  , Symbol.Cons nextH nextT tail
  , ExtractWordGo nextH nextT acc' word rest
  ) =>
  ExtractWordGo h tail acc word rest

-- After a column name + space + word: if AS, skip alias; otherwise error
class HandleAfterColumnWord :: Symbol -> Symbol -> Row Type -> Constraint
class HandleAfterColumnWord word rest cols

-- AS alias: skip the alias, then expect comma or end
instance SkipAlias rest cols => HandleAfterColumnWord "AS" rest cols
else instance SkipAlias rest cols => HandleAfterColumnWord "as" rest cols

-- Skip the alias word, then expect comma or end of string
class SkipAlias :: Symbol -> Row Type -> Constraint
class SkipAlias sym cols

instance SkipAlias "" cols
else instance
  ( ExtractWord sym _alias afterAlias
  , SkipSpaces afterAlias rest
  , ExpectCommaOrEnd rest cols
  ) =>
  SkipAlias sym cols

class ExpectCommaOrEnd :: Symbol -> Row Type -> Constraint
class ExpectCommaOrEnd sym cols

instance ExpectCommaOrEnd "" cols
else instance
  ( Symbol.Cons h t sym
  , ExpectCommaOrEndByHead h t cols
  ) =>
  ExpectCommaOrEnd sym cols

class ExpectCommaOrEndByHead :: Symbol -> Symbol -> Row Type -> Constraint
class ExpectCommaOrEndByHead head tail cols

instance
  ( SkipSpaces tail rest
  , ValidateColumns rest cols
  ) =>
  ExpectCommaOrEndByHead "," tail cols

-- Skip leading spaces
class SkipSpaces :: Symbol -> Symbol -> Constraint
class SkipSpaces sym result | sym -> result

instance SkipSpaces "" ""
else instance
  ( Symbol.Cons head tail sym
  , SkipSpacesGo head tail result
  ) =>
  SkipSpaces sym result

class SkipSpacesGo :: Symbol -> Symbol -> Symbol -> Constraint
class SkipSpacesGo head tail result | head tail -> result

instance SkipSpaces tail result => SkipSpacesGo " " tail result
else instance Symbol.Cons head tail result => SkipSpacesGo head tail result

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Type utilities
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class UnwrapMaybe :: Type -> Type -> Constraint
class UnwrapMaybe a b | a -> b

instance UnwrapMaybe (Maybe a) a
else instance UnwrapMaybe a a

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- StripColumns: (name :: Column String None, ...) → (name :: String, ...)
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class StripColumnsRL :: RL.RowList Type -> RL.RowList Type -> Constraint
class StripColumnsRL rl out | rl -> out

instance StripColumnsRL RL.Nil RL.Nil
instance StripColumnsRL tail out' => StripColumnsRL (RL.Cons name (Column typ constraints) tail) (RL.Cons name typ out')

class StripColumns :: Row Type -> Row Type -> Constraint
class StripColumns cols result | cols -> result

instance (RowToList cols rl, StripColumnsRL rl outRL, ListToRow outRL result) => StripColumns cols result

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- ParseSelect: parse "name, email AS e" → RowList (name :: String, e :: String)
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class ParseSelect :: Symbol -> Row Type -> Row Type -> Constraint
class ParseSelect sym cols result | sym cols -> result

instance ParseSelect "" cols ()
else instance
  ( Symbol.Cons h t sym
  , ParseSelectGo h t "" cols RL.Nil outRL
  , ListToRow outRL result
  ) =>
  ParseSelect sym cols result

class ParseSelectGo :: Symbol -> Symbol -> Symbol -> Row Type -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseSelectGo head tail acc cols accRL outRL | head tail acc cols accRL -> outRL

-- Comma: emit column, continue
instance
  ( Row.Cons acc (Column typ constraints) rest cols
  , SkipSpaces tail rest'
  , ParseSelectContinue rest' cols (RL.Cons acc typ accRL) outRL
  ) =>
  ParseSelectGo "," tail acc cols accRL outRL

-- Space: column name done, check for AS or comma
else instance
  ( SkipSpaces tail rest
  , ParseSelectAfterCol acc rest cols accRL outRL
  ) =>
  ParseSelectGo " " tail acc cols accRL outRL

-- End of string: emit final column
else instance
  ( Symbol.Append acc h acc'
  , Row.Cons acc' (Column typ constraints) rest cols
  ) =>
  ParseSelectGo h "" acc cols accRL (RL.Cons acc' typ accRL)

-- Regular char: accumulate
else instance
  ( Symbol.Append acc h acc'
  , Symbol.Cons nextH nextT tail
  , ParseSelectGo nextH nextT acc' cols accRL outRL
  ) =>
  ParseSelectGo h tail acc cols accRL outRL

-- After column name + space: AS alias, comma, or end
class ParseSelectAfterCol :: Symbol -> Symbol -> Row Type -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseSelectAfterCol colName rest cols accRL outRL | colName rest cols accRL -> outRL

-- End: emit column with its own name
instance
  ( Row.Cons colName (Column typ constraints) rest' cols
  ) =>
  ParseSelectAfterCol colName "" cols accRL (RL.Cons colName typ accRL)

-- Non-empty: branch on first char
else instance
  ( Symbol.Cons h t rest
  , ParseSelectAfterColByHead h t colName cols accRL outRL
  ) =>
  ParseSelectAfterCol colName rest cols accRL outRL

class ParseSelectAfterColByHead :: Symbol -> Symbol -> Symbol -> Row Type -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseSelectAfterColByHead head tail colName cols accRL outRL | head tail colName cols accRL -> outRL

-- Comma: emit column with own name, continue
instance
  ( Row.Cons colName (Column typ constraints) rest cols
  , SkipSpaces tail rest'
  , ParseSelectContinue rest' cols (RL.Cons colName typ accRL) outRL
  ) =>
  ParseSelectAfterColByHead "," tail colName cols accRL outRL

-- Otherwise (AS ...): extract word
else instance
  ( Symbol.Append h t rest
  , ExtractWord rest word afterWord
  , ParseSelectHandleAS word afterWord colName cols accRL outRL
  ) =>
  ParseSelectAfterColByHead h t colName cols accRL outRL

-- Handle AS keyword: extract alias name
class ParseSelectHandleAS :: Symbol -> Symbol -> Symbol -> Row Type -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseSelectHandleAS keyword afterKeyword colName cols accRL outRL | keyword afterKeyword colName cols accRL -> outRL

instance
  ( ExtractWord afterKeyword alias afterAlias
  , Row.Cons colName (Column typ constraints) rest cols
  , SkipSpaces afterAlias rest'
  , ParseSelectExpectCommaOrEnd rest' cols (RL.Cons alias typ accRL) outRL
  ) =>
  ParseSelectHandleAS "AS" afterKeyword colName cols accRL outRL

else instance
  ( ExtractWord afterKeyword alias afterAlias
  , Row.Cons colName (Column typ constraints) rest cols
  , SkipSpaces afterAlias rest'
  , ParseSelectExpectCommaOrEnd rest' cols (RL.Cons alias typ accRL) outRL
  ) =>
  ParseSelectHandleAS "as" afterKeyword colName cols accRL outRL

class ParseSelectExpectCommaOrEnd :: Symbol -> Row Type -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseSelectExpectCommaOrEnd sym cols accRL outRL | sym cols accRL -> outRL

instance ParseSelectExpectCommaOrEnd "" cols accRL accRL
else instance
  ( Symbol.Cons h t sym
  , ParseSelectExpectCommaOrEndByHead h t cols accRL outRL
  ) =>
  ParseSelectExpectCommaOrEnd sym cols accRL outRL

class ParseSelectExpectCommaOrEndByHead :: Symbol -> Symbol -> Row Type -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseSelectExpectCommaOrEndByHead head tail cols accRL outRL | head tail cols accRL -> outRL

instance
  ( SkipSpaces tail rest
  , ParseSelectContinue rest cols accRL outRL
  ) =>
  ParseSelectExpectCommaOrEndByHead "," tail cols accRL outRL

-- Continue parsing more columns
class ParseSelectContinue :: Symbol -> Row Type -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseSelectContinue sym cols accRL outRL | sym cols accRL -> outRL

instance ParseSelectContinue "" cols accRL accRL
else instance
  ( Symbol.Cons h t sym
  , ParseSelectGo h t "" cols accRL outRL
  ) =>
  ParseSelectContinue sym cols accRL outRL

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- ParseWhere: parse "id = $id AND age > $age" → params (id :: Int, age :: Int)
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- Sentinel type for "no current column type yet"
data NoType

class ParseWhere :: Symbol -> Row Type -> Row Type -> Constraint
class ParseWhere sym cols params | sym cols -> params

instance ParseWhere "" cols ()
else instance
  ( Symbol.Cons h t sym
  , ParseWhereGo h t "" NoType cols RL.Nil outRL
  , ListToRow outRL params
  ) =>
  ParseWhere sym cols params

class ParseWhereGo :: Symbol -> Symbol -> Symbol -> Type -> Row Type -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseWhereGo head tail acc currentType cols paramsIn paramsOut | head tail acc currentType cols paramsIn -> paramsOut

-- Space: flush word, continue
instance
  ( FlushWhereWord acc currentType cols paramsIn currentType' paramsOut'
  , SkipSpaces tail rest
  , ParseWhereContinue rest currentType' cols paramsOut' paramsOut
  ) =>
  ParseWhereGo " " tail acc currentType cols paramsIn paramsOut

-- Operators: flush word, continue
else instance (FlushWhereWord acc currentType cols paramsIn currentType' paramsOut', ParseWhereContinue tail currentType' cols paramsOut' paramsOut) => ParseWhereGo "=" tail acc currentType cols paramsIn paramsOut
else instance (FlushWhereWord acc currentType cols paramsIn currentType' paramsOut', ParseWhereContinue tail currentType' cols paramsOut' paramsOut) => ParseWhereGo ">" tail acc currentType cols paramsIn paramsOut
else instance (FlushWhereWord acc currentType cols paramsIn currentType' paramsOut', ParseWhereContinue tail currentType' cols paramsOut' paramsOut) => ParseWhereGo "<" tail acc currentType cols paramsIn paramsOut
else instance (FlushWhereWord acc currentType cols paramsIn currentType' paramsOut', ParseWhereContinue tail currentType' cols paramsOut' paramsOut) => ParseWhereGo "!" tail acc currentType cols paramsIn paramsOut
else instance (FlushWhereWord acc currentType cols paramsIn currentType' paramsOut', ParseWhereContinue tail currentType' cols paramsOut' paramsOut) => ParseWhereGo "(" tail acc currentType cols paramsIn paramsOut
else instance (FlushWhereWord acc currentType cols paramsIn currentType' paramsOut', ParseWhereContinue tail currentType' cols paramsOut' paramsOut) => ParseWhereGo ")" tail acc currentType cols paramsIn paramsOut
-- JSONB / extra operators and string literals
else instance (FlushWhereWord acc currentType cols paramsIn currentType' paramsOut', ParseWhereContinue tail currentType' cols paramsOut' paramsOut) => ParseWhereGo "'" tail acc currentType cols paramsIn paramsOut
else instance (FlushWhereWord acc currentType cols paramsIn currentType' paramsOut', ParseWhereContinue tail currentType' cols paramsOut' paramsOut) => ParseWhereGo "@" tail acc currentType cols paramsIn paramsOut
else instance (FlushWhereWord acc currentType cols paramsIn currentType' paramsOut', ParseWhereContinue tail currentType' cols paramsOut' paramsOut) => ParseWhereGo "?" tail acc currentType cols paramsIn paramsOut
else instance (FlushWhereWord acc currentType cols paramsIn currentType' paramsOut', ParseWhereContinue tail currentType' cols paramsOut' paramsOut) => ParseWhereGo ":" tail acc currentType cols paramsIn paramsOut
else instance (FlushWhereWord acc currentType cols paramsIn currentType' paramsOut', ParseWhereContinue tail currentType' cols paramsOut' paramsOut) => ParseWhereGo "~" tail acc currentType cols paramsIn paramsOut
else instance (FlushWhereWord acc currentType cols paramsIn currentType' paramsOut', ParseWhereContinue tail currentType' cols paramsOut' paramsOut) => ParseWhereGo "#" tail acc currentType cols paramsIn paramsOut

-- End of string: flush final word
else instance
  ( Symbol.Append acc h acc'
  , FlushWhereWord acc' currentType cols paramsIn _ct paramsOut
  ) =>
  ParseWhereGo h "" acc currentType cols paramsIn paramsOut

-- Regular char: accumulate
else instance
  ( Symbol.Append acc h acc'
  , Symbol.Cons nextH nextT tail
  , ParseWhereGo nextH nextT acc' currentType cols paramsIn paramsOut
  ) =>
  ParseWhereGo h tail acc currentType cols paramsIn paramsOut

class ParseWhereContinue :: Symbol -> Type -> Row Type -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseWhereContinue sym currentType cols paramsIn paramsOut | sym currentType cols paramsIn -> paramsOut

instance ParseWhereContinue "" currentType cols paramsIn paramsIn
else instance
  ( Symbol.Cons h t sym
  , ParseWhereGo h t "" currentType cols paramsIn paramsOut
  ) =>
  ParseWhereContinue sym currentType cols paramsIn paramsOut

-- Flush a word in WHERE context: updates currentType and/or emits params
class FlushWhereWord :: Symbol -> Type -> Row Type -> RL.RowList Type -> Type -> RL.RowList Type -> Constraint
class FlushWhereWord word currentType cols paramsIn currentTypeOut paramsOut | word currentType cols paramsIn -> currentTypeOut paramsOut

-- Empty: pass through
instance FlushWhereWord "" currentType cols paramsIn currentType paramsIn

-- SQL keywords: pass through
else instance FlushWhereWord "AND" currentType cols paramsIn currentType paramsIn
else instance FlushWhereWord "OR" currentType cols paramsIn currentType paramsIn
else instance FlushWhereWord "NOT" currentType cols paramsIn currentType paramsIn
else instance FlushWhereWord "IS" currentType cols paramsIn currentType paramsIn
else instance FlushWhereWord "NULL" currentType cols paramsIn currentType paramsIn
else instance FlushWhereWord "LIKE" currentType cols paramsIn currentType paramsIn
else instance FlushWhereWord "ILIKE" currentType cols paramsIn currentType paramsIn
else instance FlushWhereWord "IN" currentType cols paramsIn (Array currentType) paramsIn
else instance FlushWhereWord "TRUE" currentType cols paramsIn currentType paramsIn
else instance FlushWhereWord "FALSE" currentType cols paramsIn currentType paramsIn
else instance FlushWhereWord "BETWEEN" currentType cols paramsIn currentType paramsIn
else instance FlushWhereWord "ANY" currentType cols paramsIn currentType paramsIn
else instance FlushWhereWord "ALL" currentType cols paramsIn currentType paramsIn
else instance FlushWhereWord "CAST" currentType cols paramsIn currentType paramsIn
else instance FlushWhereWord "AS" currentType cols paramsIn currentType paramsIn
else instance FlushWhereWord "EXISTS" currentType cols paramsIn currentType paramsIn
-- Postgres type names (for ::type casts)
else instance FlushWhereWord "text" currentType cols paramsIn currentType paramsIn
else instance FlushWhereWord "integer" currentType cols paramsIn currentType paramsIn
else instance FlushWhereWord "bigint" currentType cols paramsIn currentType paramsIn
else instance FlushWhereWord "boolean" currentType cols paramsIn currentType paramsIn
else instance FlushWhereWord "jsonb" currentType cols paramsIn currentType paramsIn
else instance FlushWhereWord "json" currentType cols paramsIn currentType paramsIn
else instance FlushWhereWord "timestamptz" currentType cols paramsIn currentType paramsIn
else instance FlushWhereWord "timestamp" currentType cols paramsIn currentType paramsIn
else instance FlushWhereWord "date" currentType cols paramsIn currentType paramsIn
else instance FlushWhereWord "int" currentType cols paramsIn currentType paramsIn
else instance FlushWhereWord "varchar" currentType cols paramsIn currentType paramsIn
else instance FlushWhereWord "uuid" currentType cols paramsIn currentType paramsIn

-- Non-keyword: check first char
else instance
  ( Symbol.Cons head rest word
  , FlushWhereWordByHead head word currentType cols paramsIn currentTypeOut paramsOut
  ) =>
  FlushWhereWord word currentType cols paramsIn currentTypeOut paramsOut

class FlushWhereWordByHead :: Symbol -> Symbol -> Type -> Row Type -> RL.RowList Type -> Type -> RL.RowList Type -> Constraint
class FlushWhereWordByHead head word currentType cols paramsIn currentTypeOut paramsOut | head word currentType cols paramsIn -> currentTypeOut paramsOut

-- $param: extract param name (rest after $), emit with currentType
instance
  ( Symbol.Cons "$" paramName word
  ) =>
  FlushWhereWordByHead "$" word currentType cols paramsIn currentType (RL.Cons paramName currentType paramsIn)

-- Digit: number literal, pass through
else instance FlushWhereWordByHead "0" word currentType cols paramsIn currentType paramsIn
else instance FlushWhereWordByHead "1" word currentType cols paramsIn currentType paramsIn
else instance FlushWhereWordByHead "2" word currentType cols paramsIn currentType paramsIn
else instance FlushWhereWordByHead "3" word currentType cols paramsIn currentType paramsIn
else instance FlushWhereWordByHead "4" word currentType cols paramsIn currentType paramsIn
else instance FlushWhereWordByHead "5" word currentType cols paramsIn currentType paramsIn
else instance FlushWhereWordByHead "6" word currentType cols paramsIn currentType paramsIn
else instance FlushWhereWordByHead "7" word currentType cols paramsIn currentType paramsIn
else instance FlushWhereWordByHead "8" word currentType cols paramsIn currentType paramsIn
else instance FlushWhereWordByHead "9" word currentType cols paramsIn currentType paramsIn

-- Column name: look up type, strip Maybe, set as currentType
else instance
  ( Row.Cons word (Column typ constraints) rest cols
  , UnwrapMaybe typ unwrapped
  ) =>
  FlushWhereWordByHead head word currentType cols paramsIn unwrapped paramsIn

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Query builder
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

newtype Q :: Symbol -> Row Type -> Row Type -> Row Type -> Row Type -> Type
newtype Q name cols result params stage = Q { sql :: String, values :: Array PG.PGValue }

class HasClause :: Symbol -> Row Type -> Constraint
class HasClause label row

instance Row.Cons label Unit rest row => HasClause label row

from :: forall name cols. Proxy (Table name cols) -> Q name cols () () ()
from _ = Q { sql: "", values: [] }

selectAll
  :: forall name cols result r p stage stage'
   . IsSymbol name
  => StripColumns cols result
  => Row.Lacks "select" stage
  => Row.Lacks "insert" stage
  => Row.Lacks "set" stage
  => Row.Lacks "delete" stage
  => Row.Lacks "where" stage
  => Row.Lacks "orderBy" stage
  => Row.Lacks "limit" stage
  => Row.Lacks "offset" stage
  => Row.Cons "select" Unit stage stage'
  => Q name cols r p stage
  -> Q name cols result p stage'
selectAll _ = Q { sql: "SELECT * FROM " <> reflectSymbol (Proxy :: Proxy name), values: [] }

select
  :: forall @sel name cols result r p stage stage'
   . IsSymbol name
  => IsSymbol sel
  => ParseSelect sel cols result
  => Row.Lacks "select" stage
  => Row.Lacks "insert" stage
  => Row.Lacks "set" stage
  => Row.Lacks "delete" stage
  => Row.Lacks "where" stage
  => Row.Lacks "orderBy" stage
  => Row.Lacks "limit" stage
  => Row.Lacks "offset" stage
  => Row.Cons "select" Unit stage stage'
  => Q name cols r p stage
  -> Q name cols result p stage'
select _ = Q { sql: "SELECT " <> reflectSymbol (Proxy :: Proxy sel) <> " FROM " <> reflectSymbol (Proxy :: Proxy name), values: [] }

where_
  :: forall @whr name cols result params p stage stage'
   . IsSymbol whr
  => ParseWhere whr cols params
  => Row.Lacks "where" stage
  => Row.Lacks "insert" stage
  => Row.Lacks "orderBy" stage
  => Row.Lacks "limit" stage
  => Row.Lacks "offset" stage
  => Row.Cons "where" Unit stage stage'
  => Q name cols result p stage
  -> Q name cols result params stage'
where_ (Q q) = Q (q { sql = q.sql <> " WHERE " <> reflectSymbol (Proxy :: Proxy whr) })

toSQL :: forall name cols result params stage. Q name cols result params stage -> String
toSQL (Q q) = q.sql

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Type-level IsAutoGenerated (Boolean kind)
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class IsAutoGeneratedTC :: Type -> Boolean -> Constraint
class IsAutoGeneratedTC constraints result | constraints -> result

instance IsAutoGeneratedTC AutoIncrement True
else instance IsAutoGeneratedTC (Default a) True
else instance IsAutoGeneratedTC (DefaultInt a) True
else instance IsAutoGeneratedTC (DefaultBool a) True
else instance (IsAutoGeneratedTC a ra, IsAutoGeneratedTC b rb, Or ra rb result) => IsAutoGeneratedTC (a /\ b) result
else instance IsAutoGeneratedTC a False

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- InsertableColumnsRL: filter out auto-generated columns
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class InsertableColumnsRL :: RL.RowList Type -> RL.RowList Type -> Constraint
class InsertableColumnsRL tableRL outRL | tableRL -> outRL

instance InsertableColumnsRL RL.Nil RL.Nil
instance
  ( IsAutoGeneratedTC constraints isAuto
  , InsertableColumnDecide isAuto name typ tail outRL
  ) =>
  InsertableColumnsRL (RL.Cons name (Column typ constraints) tail) outRL

class InsertableColumnDecide :: Boolean -> Symbol -> Type -> RL.RowList Type -> RL.RowList Type -> Constraint
class InsertableColumnDecide isAuto name typ tail outRL | isAuto name typ tail -> outRL

instance InsertableColumnsRL tail outRL => InsertableColumnDecide True name typ tail outRL
instance InsertableColumnsRL tail outRL => InsertableColumnDecide False name typ tail (RL.Cons name typ outRL)

-- Split insertable columns into required (non-Maybe) and optional (Maybe)
class RequiredColumnsRL :: RL.RowList Type -> RL.RowList Type -> Constraint
class RequiredColumnsRL rl out | rl -> out

instance RequiredColumnsRL RL.Nil RL.Nil
instance RequiredColumnsRL tail out => RequiredColumnsRL (RL.Cons name (Maybe a) tail) out
else instance RequiredColumnsRL tail out => RequiredColumnsRL (RL.Cons name typ tail) (RL.Cons name typ out)

-- Generate column names from a RowList
class ColumnNamesRL :: RL.RowList Type -> Constraint
class ColumnNamesRL rl where
  columnNamesRL :: Proxy rl -> Array String

instance ColumnNamesRL RL.Nil where
  columnNamesRL _ = []

instance (IsSymbol name, ColumnNamesRL tail) => ColumnNamesRL (RL.Cons name typ tail) where
  columnNamesRL _ = [ reflectSymbol (Proxy :: Proxy name) ] <> columnNamesRL (Proxy :: Proxy tail)

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- RecordValuesRL: extract record values in RowList order
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class FieldToPGValue :: Type -> Constraint
class FieldToPGValue a where
  fieldToPGValue :: a -> PG.PGValue

instance FieldToPGValue a => FieldToPGValue (Maybe a) where
  fieldToPGValue = toNullable >>> unsafeCoerce
else instance FieldToPGValue a where
  fieldToPGValue = unsafeCoerce

class RecordValuesRL :: RL.RowList Type -> Row Type -> Constraint
class RecordValuesRL rl row where
  recordValuesRL :: Proxy rl -> { | row } -> Array PG.PGValue

instance RecordValuesRL RL.Nil row where
  recordValuesRL _ _ = []

instance
  ( IsSymbol name
  , Row.Cons name typ rest row
  , FieldToPGValue typ
  , RecordValuesRL tail row
  ) =>
  RecordValuesRL (RL.Cons name typ tail) row where
  recordValuesRL _ rec =
    [ fieldToPGValue (Record.get (Proxy :: Proxy name) rec) ]
      <> recordValuesRL (Proxy :: Proxy tail) rec

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- INSERT builder
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

insert
  :: forall name cols colsRL insertableRL insertable requiredRL required
       optionalProvided missing userRow userRowRL stage stage'
   . RowToList cols colsRL
  => InsertableColumnsRL colsRL insertableRL
  => ListToRow insertableRL insertable
  => RequiredColumnsRL insertableRL requiredRL
  => ListToRow requiredRL required
  => Row.Union required optionalProvided userRow
  => Row.Union userRow missing insertable
  => RowToList userRow userRowRL
  => ColumnNamesRL userRowRL
  => RecordValuesRL userRowRL userRow
  => IsSymbol name
  => Row.Lacks "select" stage
  => Row.Lacks "insert" stage
  => Row.Lacks "set" stage
  => Row.Lacks "delete" stage
  => Row.Cons "insert" Unit stage stage'
  => { | userRow }
  -> Q name cols () () stage
  -> Q name cols () () stage'
insert rec _ = Q { sql, values }
  where
  tableName = reflectSymbol (Proxy :: Proxy name)
  colNames = columnNamesRL (Proxy :: Proxy userRowRL)
  placeholders = colNames # mapWithIndex \i _ -> "$" <> show (i + 1)
  sql = "INSERT INTO " <> tableName
    <> " ("
    <> intercalate ", " colNames
    <> ")"
    <> " VALUES ("
    <> intercalate ", " placeholders
    <> ")"
  values = recordValuesRL (Proxy :: Proxy userRowRL) rec

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- RETURNING clause
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

returning
  :: forall @sel name cols result p stage stage'
   . IsSymbol sel
  => ParseSelect sel cols result
  => Row.Lacks "returning" stage
  => Row.Lacks "select" stage
  => Row.Cons "returning" Unit stage stage'
  => Q name cols () p stage
  -> Q name cols result p stage'
returning (Q q) = Q (q { sql = q.sql <> " RETURNING " <> reflectSymbol (Proxy :: Proxy sel) })

returningAll
  :: forall name cols result p stage stage'
   . StripColumns cols result
  => Row.Lacks "returning" stage
  => Row.Lacks "select" stage
  => Row.Cons "returning" Unit stage stage'
  => Q name cols () p stage
  -> Q name cols result p stage'
returningAll (Q q) = Q (q { sql = q.sql <> " RETURNING *" })

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- UPDATE builder (SET)
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class ValidateSetColumnsRL :: RL.RowList Type -> Row Type -> Constraint
class ValidateSetColumnsRL rl cols

instance ValidateSetColumnsRL RL.Nil cols
instance
  ( Row.Cons name (Column typ constraints) rest cols
  , ValidateSetColumnsRL tail cols
  ) =>
  ValidateSetColumnsRL (RL.Cons name typ tail) cols

class SetClauseRL :: RL.RowList Type -> Constraint
class SetClauseRL rl where
  setClauseRL :: Proxy rl -> Int -> Array String

instance SetClauseRL RL.Nil where
  setClauseRL _ _ = []

instance (IsSymbol name, SetClauseRL tail) => SetClauseRL (RL.Cons name typ tail) where
  setClauseRL _ idx =
    [ reflectSymbol (Proxy :: Proxy name) <> " = $" <> show idx ]
      <> setClauseRL (Proxy :: Proxy tail) (idx + 1)

set
  :: forall name cols setRow setRL stage stage'
   . RowToList setRow setRL
  => IsSymbol name
  => ValidateSetColumnsRL setRL cols
  => SetClauseRL setRL
  => RecordValuesRL setRL setRow
  => Row.Lacks "select" stage
  => Row.Lacks "insert" stage
  => Row.Lacks "set" stage
  => Row.Lacks "delete" stage
  => Row.Cons "set" Unit stage stage'
  => { | setRow }
  -> Q name cols () () stage
  -> Q name cols () () stage'
set rec _ = Q { sql, values }
  where
  tableName = reflectSymbol (Proxy :: Proxy name)
  setClauses = setClauseRL (Proxy :: Proxy setRL) 1
  sql = "UPDATE " <> tableName <> " SET " <> intercalate ", " setClauses
  values = recordValuesRL (Proxy :: Proxy setRL) rec

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- DELETE builder
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

delete
  :: forall name cols r p stage stage'
   . IsSymbol name
  => Row.Lacks "select" stage
  => Row.Lacks "insert" stage
  => Row.Lacks "set" stage
  => Row.Lacks "delete" stage
  => Row.Cons "delete" Unit stage stage'
  => Q name cols r p stage
  -> Q name cols () () stage'
delete _ = Q { sql: "DELETE FROM " <> reflectSymbol (Proxy :: Proxy name), values: [] }

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- ORDER BY / LIMIT / OFFSET
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

orderBy
  :: forall @cols name tableCols result params stage stage'
   . IsSymbol cols
  => ValidateOrderBy cols tableCols
  => HasClause "select" stage
  => Row.Lacks "orderBy" stage
  => Row.Lacks "limit" stage
  => Row.Lacks "offset" stage
  => Row.Cons "orderBy" Unit stage stage'
  => Q name tableCols result params stage
  -> Q name tableCols result params stage'
orderBy (Q q) = Q (q { sql = q.sql <> " ORDER BY " <> reflectSymbol (Proxy :: Proxy cols) })

limit
  :: forall name cols result params stage stage'
   . HasClause "select" stage
  => Row.Lacks "limit" stage
  => Row.Cons "limit" Unit stage stage'
  => Int
  -> Q name cols result params stage
  -> Q name cols result params stage'
limit n (Q q) = Q (q { sql = q.sql <> " LIMIT " <> show n })

offset
  :: forall name cols result params stage stage'
   . HasClause "select" stage
  => Row.Lacks "offset" stage
  => Row.Cons "offset" Unit stage stage'
  => Int
  -> Q name cols result params stage
  -> Q name cols result params stage'
offset n (Q q) = Q (q { sql = q.sql <> " OFFSET " <> show n })

-- Validate ORDER BY columns: "name, age DESC" → validate name, age exist
class ValidateOrderBy :: Symbol -> Row Type -> Constraint
class ValidateOrderBy sym cols

instance ValidateOrderBy "" cols
else instance
  ( Symbol.Cons h t sym
  , ValidateOrderByGo h t "" cols
  ) =>
  ValidateOrderBy sym cols

class ValidateOrderByGo :: Symbol -> Symbol -> Symbol -> Row Type -> Constraint
class ValidateOrderByGo head tail acc cols

-- Comma: flush column, continue
instance
  ( FlushOrderByWord acc cols
  , SkipSpaces tail rest
  , ValidateOrderBy rest cols
  ) =>
  ValidateOrderByGo "," tail acc cols

-- Space: flush column, skip modifiers (ASC/DESC/NULLS FIRST/NULLS LAST)
else instance
  ( SkipSpaces tail rest
  , FlushOrderByThenSkipModifiers acc rest cols
  ) =>
  ValidateOrderByGo " " tail acc cols

-- End of string: flush final column
else instance
  ( Symbol.Append acc h acc'
  , FlushOrderByWord acc' cols
  ) =>
  ValidateOrderByGo h "" acc cols

-- Regular char: accumulate
else instance
  ( Symbol.Append acc h acc'
  , Symbol.Cons nextH nextT tail
  , ValidateOrderByGo nextH nextT acc' cols
  ) =>
  ValidateOrderByGo h tail acc cols

class FlushOrderByWord :: Symbol -> Row Type -> Constraint
class FlushOrderByWord word cols

instance FlushOrderByWord "" cols
else instance FlushOrderByWord "ASC" cols
else instance FlushOrderByWord "asc" cols
else instance FlushOrderByWord "DESC" cols
else instance FlushOrderByWord "desc" cols
else instance FlushOrderByWord "NULLS" cols
else instance FlushOrderByWord "FIRST" cols
else instance FlushOrderByWord "LAST" cols
else instance Row.Cons word (Column typ constraints) rest cols => FlushOrderByWord word cols

class FlushOrderByThenSkipModifiers :: Symbol -> Symbol -> Row Type -> Constraint
class FlushOrderByThenSkipModifiers colName rest cols

-- End of input
instance FlushOrderByWord colName cols => FlushOrderByThenSkipModifiers colName "" cols

-- Non-empty: branch on first char
else instance
  ( FlushOrderByWord colName cols
  , Symbol.Cons h t rest
  , FlushOrderByThenSkipModifiersByHead h t cols
  ) =>
  FlushOrderByThenSkipModifiers colName rest cols

class FlushOrderByThenSkipModifiersByHead :: Symbol -> Symbol -> Row Type -> Constraint
class FlushOrderByThenSkipModifiersByHead head tail cols

-- Comma: continue with next column
instance
  ( SkipSpaces tail rest
  , ValidateOrderBy rest cols
  ) =>
  FlushOrderByThenSkipModifiersByHead "," tail cols

-- Other: word is a modifier (ASC/DESC/etc), keep consuming
else instance
  ( Symbol.Append h t rest
  , ExtractWord rest word afterWord
  , FlushOrderByWord word cols
  , SkipSpaces afterWord rest'
  , ValidateOrderByContinue rest' cols
  ) =>
  FlushOrderByThenSkipModifiersByHead h t cols

class ValidateOrderByContinue :: Symbol -> Row Type -> Constraint
class ValidateOrderByContinue sym cols

instance ValidateOrderByContinue "" cols
else instance
  ( Symbol.Cons h t sym
  , ValidateOrderByContinueByHead h t cols
  ) =>
  ValidateOrderByContinue sym cols

class ValidateOrderByContinueByHead :: Symbol -> Symbol -> Row Type -> Constraint
class ValidateOrderByContinueByHead head tail cols

instance
  ( SkipSpaces tail rest
  , ValidateOrderBy rest cols
  ) =>
  ValidateOrderByContinueByHead "," tail cols

-- More modifiers (e.g. "NULLS FIRST" after "DESC")
else instance
  ( Symbol.Append h t rest
  , ExtractWord rest word afterWord
  , FlushOrderByWord word cols
  , SkipSpaces afterWord rest'
  , ValidateOrderByContinue rest' cols
  ) =>
  ValidateOrderByContinueByHead h t cols

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- ON CONFLICT
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- ParseConflictAction: validate "DO UPDATE SET col = EXCLUDED.col, ..."
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class ParseConflictAction :: Symbol -> Row Type -> Constraint
class ParseConflictAction sym cols

instance
  ( ExtractWord sym w1 rest1
  , ExpectKeyword w1 "DO"
  , ExtractWord rest1 w2 rest2
  , ExpectKeyword w2 "UPDATE"
  , ExtractWord rest2 w3 rest3
  , ExpectKeyword w3 "SET"
  , ParseAssignments rest3 cols
  ) =>
  ParseConflictAction sym cols

class ExpectKeyword :: Symbol -> Symbol -> Constraint
class ExpectKeyword actual expected

instance ExpectKeyword a a
else instance
  Fail (Beside (Beside (Text "Expected keyword ") (Quote expected)) (Beside (Text " but got ") (Quote actual))) =>
  ExpectKeyword actual expected

-- Parse "col = EXCLUDED.col, col2 = EXCLUDED.col2"
class ParseAssignments :: Symbol -> Row Type -> Constraint
class ParseAssignments sym cols

instance ParseAssignments "" cols
else instance
  ( ExtractWord sym colName rest1
  , Row.Cons colName colType colRest cols
  , SkipSpaces rest1 rest2
  , ExpectChar rest2 "=" rest3
  , SkipSpaces rest3 rest4
  , ExtractWord rest4 excRef rest5
  , ValidateExcludedRef excRef colName
  , SkipSpaces rest5 rest6
  , ParseAssignmentsContinue rest6 cols
  ) =>
  ParseAssignments sym cols

class ParseAssignmentsContinue :: Symbol -> Row Type -> Constraint
class ParseAssignmentsContinue sym cols

instance ParseAssignmentsContinue "" cols
else instance
  ( Symbol.Cons h t sym
  , ParseAssignmentsContinueByHead h t cols
  ) =>
  ParseAssignmentsContinue sym cols

class ParseAssignmentsContinueByHead :: Symbol -> Symbol -> Row Type -> Constraint
class ParseAssignmentsContinueByHead head tail cols

instance
  ( SkipSpaces tail rest
  , ParseAssignments rest cols
  ) =>
  ParseAssignmentsContinueByHead "," tail cols

class ExpectChar :: Symbol -> Symbol -> Symbol -> Constraint
class ExpectChar sym char rest | sym char -> rest

instance
  ( Symbol.Cons h t sym
  , ExpectCharMatch h t char rest
  ) =>
  ExpectChar sym char rest

class ExpectCharMatch :: Symbol -> Symbol -> Symbol -> Symbol -> Constraint
class ExpectCharMatch head tail expected rest | head tail expected -> rest

instance ExpectCharMatch c tail c tail
else instance
  Fail (Beside (Beside (Text "Expected '") (Quote expected)) (Beside (Text "' but got '") (Quote head))) =>
  ExpectCharMatch head tail expected rest

-- Validate "EXCLUDED.col" matches the column name
class ValidateExcludedRef :: Symbol -> Symbol -> Constraint
class ValidateExcludedRef ref colName

instance
  ( Symbol.Append "EXCLUDED." colName expected
  , MatchSymbol ref expected
  ) =>
  ValidateExcludedRef ref colName

class MatchSymbol :: Symbol -> Symbol -> Constraint
class MatchSymbol a b

instance MatchSymbol a a
else instance
  Fail (Beside (Beside (Text "Expected ") (Quote expected)) (Beside (Text " but got ") (Quote actual))) =>
  MatchSymbol actual expected

onConflict
  :: forall @target @action name cols result params stage stage'
   . IsSymbol target
  => IsSymbol action
  => ValidateColumns target cols
  => ParseConflictAction action cols
  => HasClause "insert" stage
  => Row.Lacks "conflict" stage
  => Row.Cons "conflict" Unit stage stage'
  => Q name cols result params stage
  -> Q name cols result params stage'
onConflict (Q q) = Q
  ( q
      { sql = q.sql
          <> " ON CONFLICT ("
          <> reflectSymbol (Proxy :: Proxy target)
          <> ") "
          <> reflectSymbol (Proxy :: Proxy action)
      }
  )

onConflictDoNothing
  :: forall @target name cols result params stage stage'
   . IsSymbol target
  => ValidateColumns target cols
  => HasClause "insert" stage
  => Row.Lacks "conflict" stage
  => Row.Cons "conflict" Unit stage stage'
  => Q name cols result params stage
  -> Q name cols result params stage'
onConflictDoNothing (Q q) = Q
  ( q
      { sql = q.sql
          <> " ON CONFLICT ("
          <> reflectSymbol (Proxy :: Proxy target)
          <> ") DO NOTHING"
      }
  )

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Query execution
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class ParamsToArray :: RL.RowList Type -> Row Type -> Constraint
class ParamsToArray rl row where
  paramsToArray :: Proxy rl -> { | row } -> Array { name :: String, value :: Foreign }

instance ParamsToArray RL.Nil row where
  paramsToArray _ _ = []

instance
  ( IsSymbol name
  , Row.Cons name typ rest row
  , Row.Lacks name rest
  , ParamsToArray tail row
  ) =>
  ParamsToArray (RL.Cons name typ tail) row where
  paramsToArray _ rec =
    [ { name: reflectSymbol (Proxy :: Proxy name)
      , value: unsafeToForeign (Record.get (Proxy :: Proxy name) rec)
      }
    ]
      <> paramsToArray (Proxy :: Proxy tail) rec

namedParamRegex :: Regex.Regex
namedParamRegex = case Regex.regex "\\$[a-zA-Z_][a-zA-Z0-9_]*" Regex.global of
  Right r -> r
  Left _ -> unsafeCoerce unit

replaceNamedParams :: Int -> Array { name :: String, value :: Foreign } -> String -> { sql :: String, values :: Array PG.PGValue }
replaceNamedParams offset entries sql = do
  let indexed = entries # mapWithIndex \i e -> { idx: offset + i + 1, name: e.name, value: e.value }
  let replacements = indexed # foldl (\m e -> Map.insert ("$" <> e.name) ("$" <> show e.idx) m) Map.empty
  let
    sql' = Regex.replace' namedParamRegex
      ( \match _ -> case Map.lookup match replacements of
          Nothing -> match
          Just v -> v
      )
      sql
  { sql: sql', values: map (\e -> unsafeCoerce e.value) indexed }

runQuery
  :: forall name cols result params paramsRL stage
   . RowToList params paramsRL
  => ParamsToArray paramsRL params
  => ReadForeign { | result }
  => PG.Connection
  -> { | params }
  -> Q name cols result params stage
  -> Aff (Array { | result })
runQuery conn params (Q q) = do
  let entries = paramsToArray (Proxy :: Proxy paramsRL) params
  let { sql, values } = replaceNamedParams (Array.length q.values) entries q.sql
  let allValues = q.values <> values
  result <- PG.query (PG.SQL sql) allValues conn
  pure (unsafeDecodeRows result.rows)

runQueryOne
  :: forall name cols result params paramsRL stage
   . RowToList params paramsRL
  => ParamsToArray paramsRL params
  => ReadForeign { | result }
  => PG.Connection
  -> { | params }
  -> Q name cols result params stage
  -> Aff (Maybe { | result })
runQueryOne conn params (Q q) = do
  let entries = paramsToArray (Proxy :: Proxy paramsRL) params
  let { sql, values } = replaceNamedParams (Array.length q.values) entries q.sql
  let allValues = q.values <> values
  result <- PG.queryOne (PG.SQL sql) allValues conn
  pure (result <#> unsafeDecodeRow)

runExecute
  :: forall name cols params paramsRL stage
   . RowToList params paramsRL
  => ParamsToArray paramsRL params
  => PG.Connection
  -> { | params }
  -> Q name cols () params stage
  -> Aff Int
runExecute conn params (Q q) = do
  let entries = paramsToArray (Proxy :: Proxy paramsRL) params
  let { sql, values } = replaceNamedParams (Array.length q.values) entries q.sql
  let allValues = q.values <> values
  PG.execute (PG.SQL sql) allValues conn

-- Transaction variants

runQueryTx
  :: forall name cols result params paramsRL stage
   . RowToList params paramsRL
  => ParamsToArray paramsRL params
  => ReadForeign { | result }
  => PG.Transaction
  -> { | params }
  -> Q name cols result params stage
  -> Aff (Array { | result })
runQueryTx txn params (Q q) = do
  let entries = paramsToArray (Proxy :: Proxy paramsRL) params
  let { sql, values } = replaceNamedParams (Array.length q.values) entries q.sql
  let allValues = q.values <> values
  result <- PG.txQuery (PG.SQL sql) allValues txn
  pure (unsafeDecodeRows result.rows)

runExecuteTx
  :: forall name cols params paramsRL stage
   . RowToList params paramsRL
  => ParamsToArray paramsRL params
  => PG.Transaction
  -> { | params }
  -> Q name cols () params stage
  -> Aff Int
runExecuteTx txn params (Q q) = do
  let entries = paramsToArray (Proxy :: Proxy paramsRL) params
  let { sql, values } = replaceNamedParams (Array.length q.values) entries q.sql
  let allValues = q.values <> values
  PG.txExecute (PG.SQL sql) allValues txn

unsafeDecodeRows :: forall a. ReadForeign a => Array Foreign -> Array a
unsafeDecodeRows = map unsafeDecodeRow

unsafeDecodeRow :: forall a. ReadForeign a => Foreign -> a
unsafeDecodeRow f = case runExcept (readImpl f) of
  Left _ -> unsafeCoerce unit
  Right a -> a
