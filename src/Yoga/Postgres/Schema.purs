module Yoga.Postgres.Schema where

import Prelude

import Data.Array as Array
import Data.Array (intercalate, mapWithIndex, foldl)
import Data.DateTime (DateTime)
import Data.JSDate as JSDate
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

instance ReadForeign Jsonb where
  readImpl = pure <<< Jsonb

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
else instance FlushWhereWord "ANY" currentType cols paramsIn (Array currentType) paramsIn
else instance FlushWhereWord "ALL" currentType cols paramsIn (Array currentType) paramsIn
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
else instance FieldToPGValue DateTime where
  fieldToPGValue = JSDate.fromDateTime >>> unsafeCoerce
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

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- JOIN query builder
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

newtype JQ :: Row (Row Type) -> Row Type -> Row Type -> Row Type -> Type
newtype JQ tables result params stage = JQ { sql :: String, values :: Array PG.PGValue }

toSQLJQ :: forall tables result params stage. JQ tables result params stage -> String
toSQLJQ (JQ q) = q.sql

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- SplitOnDot: "users.id" → ("users", "id"); "name" → unqualified
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class SplitOnDot :: Symbol -> Boolean -> Symbol -> Symbol -> Constraint
class SplitOnDot sym hasDot table col | sym -> hasDot table col

instance
  ( Symbol.Cons h t sym
  , SplitOnDotGo h t "" hasDot table col
  ) =>
  SplitOnDot sym hasDot table col

class SplitOnDotGo :: Symbol -> Symbol -> Symbol -> Boolean -> Symbol -> Symbol -> Constraint
class SplitOnDotGo head tail acc hasDot table col | head tail acc -> hasDot table col

-- Found dot: acc is table, rest is column
instance SplitOnDotGo "." tail acc True acc tail

-- End of string without dot: unqualified
else instance
  ( Symbol.Append acc h col
  ) =>
  SplitOnDotGo h "" acc False "" col

-- Regular char: accumulate
else instance
  ( Symbol.Append acc h acc'
  , Symbol.Cons nextH nextT tail
  , SplitOnDotGo nextH nextT acc' hasDot table col
  ) =>
  SplitOnDotGo h tail acc hasDot table col

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- ResolveColumn: look up a column (qualified or unqualified) in tables
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class ResolveColumn :: Symbol -> Row (Row Type) -> Type -> Constraint
class ResolveColumn word tables typ | word tables -> typ

instance
  ( SplitOnDot word hasDot table col
  , ResolveColumnBranch hasDot table col tables typ
  ) =>
  ResolveColumn word tables typ

class ResolveColumnBranch :: Boolean -> Symbol -> Symbol -> Row (Row Type) -> Type -> Constraint
class ResolveColumnBranch hasDot table col tables typ | hasDot table col tables -> typ

-- Qualified: has dot
instance
  ( Row.Cons table tableCols restTables tables
  , Row.Cons col typ restCols tableCols
  ) =>
  ResolveColumnBranch True table col tables typ

-- Unqualified: no dot
else instance
  ( RowToList tables tablesRL
  , FindUnqualifiedColumn col tablesRL typ
  ) =>
  ResolveColumnBranch False table col tables typ

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- FindUnqualifiedColumn: search all tables for a column name
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class FindUnqualifiedColumn :: Symbol -> RL.RowList (Row Type) -> Type -> Constraint
class FindUnqualifiedColumn col tablesRL typ | col tablesRL -> typ

instance
  ( Fail (Beside (Text "Column ") (Beside (Quote col) (Text " not found in any joined table")))
  ) =>
  FindUnqualifiedColumn col RL.Nil typ

instance
  ( RowToList tableCols colsRL
  , HasColumnRL col colsRL found
  , FindUnqualifiedColumnDecide found col tableName tableCols tailTables typ
  ) =>
  FindUnqualifiedColumn col (RL.Cons tableName tableCols tailTables) typ

class HasColumnRL :: Symbol -> RL.RowList Type -> Boolean -> Constraint
class HasColumnRL col rl found | col rl -> found

instance HasColumnRL col RL.Nil False
instance HasColumnRL col (RL.Cons col typ tail) True
else instance HasColumnRL col tail found => HasColumnRL col (RL.Cons name typ tail) found

class FindUnqualifiedColumnDecide :: Boolean -> Symbol -> Symbol -> Row Type -> RL.RowList (Row Type) -> Type -> Constraint
class FindUnqualifiedColumnDecide found col tableName tableCols restTables typ | found col tableName tableCols restTables -> typ

-- Found in this table: verify it's not in remaining tables
instance
  ( Row.Cons col typ restCols tableCols
  , AssertNotInRemainingTables col restTables
  ) =>
  FindUnqualifiedColumnDecide True col tableName tableCols restTables typ

-- Not found in this table: keep searching
instance
  FindUnqualifiedColumn col restTables typ =>
  FindUnqualifiedColumnDecide False col tableName tableCols restTables typ

class AssertNotInRemainingTables :: Symbol -> RL.RowList (Row Type) -> Constraint
class AssertNotInRemainingTables col tablesRL

instance AssertNotInRemainingTables col RL.Nil
instance
  ( RowToList tableCols colsRL
  , HasColumnRL col colsRL found
  , AssertNotAmbiguous found col tableName
  , AssertNotInRemainingTables col tail
  ) =>
  AssertNotInRemainingTables col (RL.Cons tableName tableCols tail)

class AssertNotAmbiguous :: Boolean -> Symbol -> Symbol -> Constraint
class AssertNotAmbiguous found col tableName

instance AssertNotAmbiguous False col tableName
instance
  ( Fail (Beside (Text "Column ") (Beside (Quote col) (Text " is ambiguous - qualify with table name")))
  ) =>
  AssertNotAmbiguous True col tableName

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- ValidateJoinCondition: validate ON clause column references
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class ValidateJoinCondition :: Symbol -> Row (Row Type) -> Constraint
class ValidateJoinCondition sym tables

instance ValidateJoinCondition "" tables
else instance
  ( Symbol.Cons h t sym
  , ValidateJoinCondGo h t "" tables
  ) =>
  ValidateJoinCondition sym tables

class ValidateJoinCondGo :: Symbol -> Symbol -> Symbol -> Row (Row Type) -> Constraint
class ValidateJoinCondGo head tail acc tables

-- Space: flush word, continue
instance
  ( FlushJoinWord acc tables
  , SkipSpaces tail rest
  , ValidateJoinCondition rest tables
  ) =>
  ValidateJoinCondGo " " tail acc tables

-- Operators
else instance (FlushJoinWord acc tables, ValidateJoinCondition tail tables) => ValidateJoinCondGo "=" tail acc tables
else instance (FlushJoinWord acc tables, ValidateJoinCondition tail tables) => ValidateJoinCondGo ">" tail acc tables
else instance (FlushJoinWord acc tables, ValidateJoinCondition tail tables) => ValidateJoinCondGo "<" tail acc tables
else instance (FlushJoinWord acc tables, ValidateJoinCondition tail tables) => ValidateJoinCondGo "!" tail acc tables
else instance (FlushJoinWord acc tables, ValidateJoinCondition tail tables) => ValidateJoinCondGo "(" tail acc tables
else instance (FlushJoinWord acc tables, ValidateJoinCondition tail tables) => ValidateJoinCondGo ")" tail acc tables

-- End of string: flush final word
else instance
  ( Symbol.Append acc h acc'
  , FlushJoinWord acc' tables
  ) =>
  ValidateJoinCondGo h "" acc tables

-- Regular char (including dot): accumulate
else instance
  ( Symbol.Append acc h acc'
  , Symbol.Cons nextH nextT tail
  , ValidateJoinCondGo nextH nextT acc' tables
  ) =>
  ValidateJoinCondGo h tail acc tables

class FlushJoinWord :: Symbol -> Row (Row Type) -> Constraint
class FlushJoinWord word tables

instance FlushJoinWord "" tables
else instance FlushJoinWord "AND" tables
else instance FlushJoinWord "OR" tables
else instance FlushJoinWord "NOT" tables
else instance FlushJoinWord "IS" tables
else instance FlushJoinWord "NULL" tables
else instance FlushJoinWord "TRUE" tables
else instance FlushJoinWord "FALSE" tables
else instance
  ( Symbol.Cons head rest word
  , FlushJoinWordByHead head word tables
  ) =>
  FlushJoinWord word tables

class FlushJoinWordByHead :: Symbol -> Symbol -> Row (Row Type) -> Constraint
class FlushJoinWordByHead head word tables

-- $-prefixed: parameter, skip
instance FlushJoinWordByHead "$" word tables
-- Digit-prefixed: number literal, skip
else instance FlushJoinWordByHead "0" word tables
else instance FlushJoinWordByHead "1" word tables
else instance FlushJoinWordByHead "2" word tables
else instance FlushJoinWordByHead "3" word tables
else instance FlushJoinWordByHead "4" word tables
else instance FlushJoinWordByHead "5" word tables
else instance FlushJoinWordByHead "6" word tables
else instance FlushJoinWordByHead "7" word tables
else instance FlushJoinWordByHead "8" word tables
else instance FlushJoinWordByHead "9" word tables
-- Column reference (qualified or unqualified): resolve
else instance ResolveColumn word tables typ => FlushJoinWordByHead head word tables

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- innerJoin: Q → JQ
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

innerJoin
  :: forall @cond name1 cols1 name2 cols2 r p tables tables' stage
   . IsSymbol name1
  => IsSymbol name2
  => IsSymbol cond
  => Row.Cons name1 cols1 () tables'
  => Row.Cons name2 cols2 tables' tables
  => ValidateJoinCondition cond tables
  => Row.Lacks "select" stage
  => Row.Lacks "insert" stage
  => Row.Lacks "set" stage
  => Row.Lacks "delete" stage
  => Proxy (Table name2 cols2)
  -> Q name1 cols1 r p stage
  -> JQ tables () () (join :: Unit)
innerJoin _ _ = JQ
  { sql: reflectSymbol (Proxy :: Proxy name1)
      <> " INNER JOIN "
      <> reflectSymbol (Proxy :: Proxy name2)
      <> " ON "
      <> reflectSymbol (Proxy :: Proxy cond)
  , values: []
  }

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- ParseSelectJQ: parse "users.name, posts.title AS t" against tables
-- Result labels: column name (after dot) or explicit AS alias
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class ParseSelectJQ :: Symbol -> Row (Row Type) -> Row Type -> Constraint
class ParseSelectJQ sym tables result | sym tables -> result

instance ParseSelectJQ "" tables ()
else instance
  ( Symbol.Cons h t sym
  , ParseSelectJQGo h t "" tables RL.Nil outRL
  , ListToRow outRL result
  ) =>
  ParseSelectJQ sym tables result

class ParseSelectJQGo :: Symbol -> Symbol -> Symbol -> Row (Row Type) -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseSelectJQGo head tail acc tables accRL outRL | head tail acc tables accRL -> outRL

-- Comma: emit column, continue
instance
  ( ResolveColumn acc tables (Column typ constraints)
  , SplitOnDot acc _hasDot _table colName
  , SkipSpaces tail rest
  , ParseSelectJQContinue rest tables (RL.Cons colName typ accRL) outRL
  ) =>
  ParseSelectJQGo "," tail acc tables accRL outRL

-- Space: column reference done, check for AS or comma
else instance
  ( SkipSpaces tail rest
  , ParseSelectJQAfterCol acc rest tables accRL outRL
  ) =>
  ParseSelectJQGo " " tail acc tables accRL outRL

-- End of string: emit final column
else instance
  ( Symbol.Append acc h acc'
  , ResolveColumn acc' tables (Column typ constraints)
  , SplitOnDot acc' _hasDot _table colName
  ) =>
  ParseSelectJQGo h "" acc tables accRL (RL.Cons colName typ accRL)

-- Regular char (including dot): accumulate
else instance
  ( Symbol.Append acc h acc'
  , Symbol.Cons nextH nextT tail
  , ParseSelectJQGo nextH nextT acc' tables accRL outRL
  ) =>
  ParseSelectJQGo h tail acc tables accRL outRL

-- After column name + space: AS alias, comma, or end
class ParseSelectJQAfterCol :: Symbol -> Symbol -> Row (Row Type) -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseSelectJQAfterCol colRef rest tables accRL outRL | colRef rest tables accRL -> outRL

-- End: emit column with default label (column name after dot)
instance
  ( ResolveColumn colRef tables (Column typ constraints)
  , SplitOnDot colRef _hasDot _table colName
  ) =>
  ParseSelectJQAfterCol colRef "" tables accRL (RL.Cons colName typ accRL)

-- Non-empty: branch on first char
else instance
  ( Symbol.Cons h t rest
  , ParseSelectJQAfterColByHead h t colRef tables accRL outRL
  ) =>
  ParseSelectJQAfterCol colRef rest tables accRL outRL

class ParseSelectJQAfterColByHead :: Symbol -> Symbol -> Symbol -> Row (Row Type) -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseSelectJQAfterColByHead head tail colRef tables accRL outRL | head tail colRef tables accRL -> outRL

-- Comma: emit column with default label, continue
instance
  ( ResolveColumn colRef tables (Column typ constraints)
  , SplitOnDot colRef _hasDot _table colName
  , SkipSpaces tail rest
  , ParseSelectJQContinue rest tables (RL.Cons colName typ accRL) outRL
  ) =>
  ParseSelectJQAfterColByHead "," tail colRef tables accRL outRL

-- Otherwise (AS ...): extract word
else instance
  ( Symbol.Append h t rest
  , ExtractWord rest keyword afterKeyword
  , ParseSelectJQHandleAS keyword afterKeyword colRef tables accRL outRL
  ) =>
  ParseSelectJQAfterColByHead h t colRef tables accRL outRL

class ParseSelectJQHandleAS :: Symbol -> Symbol -> Symbol -> Row (Row Type) -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseSelectJQHandleAS keyword afterKeyword colRef tables accRL outRL | keyword afterKeyword colRef tables accRL -> outRL

instance
  ( ExtractWord afterKeyword alias afterAlias
  , ResolveColumn colRef tables (Column typ constraints)
  , SkipSpaces afterAlias rest
  , ParseSelectJQExpectEnd rest tables (RL.Cons alias typ accRL) outRL
  ) =>
  ParseSelectJQHandleAS "AS" afterKeyword colRef tables accRL outRL

else instance
  ( ExtractWord afterKeyword alias afterAlias
  , ResolveColumn colRef tables (Column typ constraints)
  , SkipSpaces afterAlias rest
  , ParseSelectJQExpectEnd rest tables (RL.Cons alias typ accRL) outRL
  ) =>
  ParseSelectJQHandleAS "as" afterKeyword colRef tables accRL outRL

class ParseSelectJQExpectEnd :: Symbol -> Row (Row Type) -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseSelectJQExpectEnd sym tables accRL outRL | sym tables accRL -> outRL

instance ParseSelectJQExpectEnd "" tables accRL accRL
else instance
  ( Symbol.Cons h t sym
  , ParseSelectJQExpectEndByHead h t tables accRL outRL
  ) =>
  ParseSelectJQExpectEnd sym tables accRL outRL

class ParseSelectJQExpectEndByHead :: Symbol -> Symbol -> Row (Row Type) -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseSelectJQExpectEndByHead head tail tables accRL outRL | head tail tables accRL -> outRL

instance
  ( SkipSpaces tail rest
  , ParseSelectJQContinue rest tables accRL outRL
  ) =>
  ParseSelectJQExpectEndByHead "," tail tables accRL outRL

class ParseSelectJQContinue :: Symbol -> Row (Row Type) -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseSelectJQContinue sym tables accRL outRL | sym tables accRL -> outRL

instance ParseSelectJQContinue "" tables accRL accRL
else instance
  ( Symbol.Cons h t sym
  , ParseSelectJQGo h t "" tables accRL outRL
  ) =>
  ParseSelectJQContinue sym tables accRL outRL

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- selectJQ: SELECT for join queries
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

selectJQ
  :: forall @sel tables result r p stage stage'
   . IsSymbol sel
  => ParseSelectJQ sel tables result
  => Row.Lacks "select" stage
  => Row.Lacks "insert" stage
  => Row.Lacks "set" stage
  => Row.Lacks "delete" stage
  => Row.Cons "select" Unit stage stage'
  => JQ tables r p stage
  -> JQ tables result p stage'
selectJQ (JQ q) = JQ (q { sql = "SELECT " <> reflectSymbol (Proxy :: Proxy sel) <> " FROM " <> q.sql })

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- ParseWhereJQ: parse WHERE with qualified column support
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class ParseWhereJQ :: Symbol -> Row (Row Type) -> Row Type -> Constraint
class ParseWhereJQ sym tables params | sym tables -> params

instance ParseWhereJQ "" tables ()
else instance
  ( Symbol.Cons h t sym
  , ParseWhereJQGo h t "" NoType tables RL.Nil outRL
  , ListToRow outRL params
  ) =>
  ParseWhereJQ sym tables params

class ParseWhereJQGo :: Symbol -> Symbol -> Symbol -> Type -> Row (Row Type) -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseWhereJQGo head tail acc currentType tables paramsIn paramsOut | head tail acc currentType tables paramsIn -> paramsOut

-- Space: flush word, continue
instance
  ( FlushWhereWordJQ acc currentType tables paramsIn currentType' paramsOut'
  , SkipSpaces tail rest
  , ParseWhereJQContinue rest currentType' tables paramsOut' paramsOut
  ) =>
  ParseWhereJQGo " " tail acc currentType tables paramsIn paramsOut

-- Operators
else instance (FlushWhereWordJQ acc currentType tables paramsIn currentType' paramsOut', ParseWhereJQContinue tail currentType' tables paramsOut' paramsOut) => ParseWhereJQGo "=" tail acc currentType tables paramsIn paramsOut
else instance (FlushWhereWordJQ acc currentType tables paramsIn currentType' paramsOut', ParseWhereJQContinue tail currentType' tables paramsOut' paramsOut) => ParseWhereJQGo ">" tail acc currentType tables paramsIn paramsOut
else instance (FlushWhereWordJQ acc currentType tables paramsIn currentType' paramsOut', ParseWhereJQContinue tail currentType' tables paramsOut' paramsOut) => ParseWhereJQGo "<" tail acc currentType tables paramsIn paramsOut
else instance (FlushWhereWordJQ acc currentType tables paramsIn currentType' paramsOut', ParseWhereJQContinue tail currentType' tables paramsOut' paramsOut) => ParseWhereJQGo "!" tail acc currentType tables paramsIn paramsOut
else instance (FlushWhereWordJQ acc currentType tables paramsIn currentType' paramsOut', ParseWhereJQContinue tail currentType' tables paramsOut' paramsOut) => ParseWhereJQGo "(" tail acc currentType tables paramsIn paramsOut
else instance (FlushWhereWordJQ acc currentType tables paramsIn currentType' paramsOut', ParseWhereJQContinue tail currentType' tables paramsOut' paramsOut) => ParseWhereJQGo ")" tail acc currentType tables paramsIn paramsOut
else instance (FlushWhereWordJQ acc currentType tables paramsIn currentType' paramsOut', ParseWhereJQContinue tail currentType' tables paramsOut' paramsOut) => ParseWhereJQGo "'" tail acc currentType tables paramsIn paramsOut
else instance (FlushWhereWordJQ acc currentType tables paramsIn currentType' paramsOut', ParseWhereJQContinue tail currentType' tables paramsOut' paramsOut) => ParseWhereJQGo "@" tail acc currentType tables paramsIn paramsOut
else instance (FlushWhereWordJQ acc currentType tables paramsIn currentType' paramsOut', ParseWhereJQContinue tail currentType' tables paramsOut' paramsOut) => ParseWhereJQGo "?" tail acc currentType tables paramsIn paramsOut
else instance (FlushWhereWordJQ acc currentType tables paramsIn currentType' paramsOut', ParseWhereJQContinue tail currentType' tables paramsOut' paramsOut) => ParseWhereJQGo ":" tail acc currentType tables paramsIn paramsOut
else instance (FlushWhereWordJQ acc currentType tables paramsIn currentType' paramsOut', ParseWhereJQContinue tail currentType' tables paramsOut' paramsOut) => ParseWhereJQGo "~" tail acc currentType tables paramsIn paramsOut
else instance (FlushWhereWordJQ acc currentType tables paramsIn currentType' paramsOut', ParseWhereJQContinue tail currentType' tables paramsOut' paramsOut) => ParseWhereJQGo "#" tail acc currentType tables paramsIn paramsOut

-- End of string: flush final word
else instance
  ( Symbol.Append acc h acc'
  , FlushWhereWordJQ acc' currentType tables paramsIn _ct paramsOut
  ) =>
  ParseWhereJQGo h "" acc currentType tables paramsIn paramsOut

-- Regular char (including dot): accumulate
else instance
  ( Symbol.Append acc h acc'
  , Symbol.Cons nextH nextT tail
  , ParseWhereJQGo nextH nextT acc' currentType tables paramsIn paramsOut
  ) =>
  ParseWhereJQGo h tail acc currentType tables paramsIn paramsOut

class ParseWhereJQContinue :: Symbol -> Type -> Row (Row Type) -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseWhereJQContinue sym currentType tables paramsIn paramsOut | sym currentType tables paramsIn -> paramsOut

instance ParseWhereJQContinue "" currentType tables paramsIn paramsIn
else instance
  ( Symbol.Cons h t sym
  , ParseWhereJQGo h t "" currentType tables paramsIn paramsOut
  ) =>
  ParseWhereJQContinue sym currentType tables paramsIn paramsOut

-- Flush a word in JQ WHERE context
class FlushWhereWordJQ :: Symbol -> Type -> Row (Row Type) -> RL.RowList Type -> Type -> RL.RowList Type -> Constraint
class FlushWhereWordJQ word currentType tables paramsIn currentTypeOut paramsOut | word currentType tables paramsIn -> currentTypeOut paramsOut

instance FlushWhereWordJQ "" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordJQ "AND" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordJQ "OR" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordJQ "NOT" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordJQ "IS" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordJQ "NULL" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordJQ "LIKE" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordJQ "ILIKE" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordJQ "IN" currentType tables paramsIn (Array currentType) paramsIn
else instance FlushWhereWordJQ "TRUE" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordJQ "FALSE" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordJQ "BETWEEN" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordJQ "ANY" currentType tables paramsIn (Array currentType) paramsIn
else instance FlushWhereWordJQ "ALL" currentType tables paramsIn (Array currentType) paramsIn
else instance FlushWhereWordJQ "CAST" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordJQ "AS" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordJQ "EXISTS" currentType tables paramsIn currentType paramsIn
-- Postgres type names
else instance FlushWhereWordJQ "text" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordJQ "integer" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordJQ "bigint" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordJQ "boolean" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordJQ "jsonb" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordJQ "json" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordJQ "timestamptz" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordJQ "timestamp" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordJQ "date" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordJQ "int" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordJQ "varchar" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordJQ "uuid" currentType tables paramsIn currentType paramsIn
-- Non-keyword: check first char
else instance
  ( Symbol.Cons head rest word
  , FlushWhereWordJQByHead head word currentType tables paramsIn currentTypeOut paramsOut
  ) =>
  FlushWhereWordJQ word currentType tables paramsIn currentTypeOut paramsOut

class FlushWhereWordJQByHead :: Symbol -> Symbol -> Type -> Row (Row Type) -> RL.RowList Type -> Type -> RL.RowList Type -> Constraint
class FlushWhereWordJQByHead head word currentType tables paramsIn currentTypeOut paramsOut | head word currentType tables paramsIn -> currentTypeOut paramsOut

-- $param: emit with currentType
instance
  ( Symbol.Cons "$" paramName word
  ) =>
  FlushWhereWordJQByHead "$" word currentType tables paramsIn currentType (RL.Cons paramName currentType paramsIn)

-- Digit: number literal, pass through
else instance FlushWhereWordJQByHead "0" word currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordJQByHead "1" word currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordJQByHead "2" word currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordJQByHead "3" word currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordJQByHead "4" word currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordJQByHead "5" word currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordJQByHead "6" word currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordJQByHead "7" word currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordJQByHead "8" word currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordJQByHead "9" word currentType tables paramsIn currentType paramsIn

-- Column reference: resolve and set as currentType
else instance
  ( ResolveColumn word tables (Column typ constraints)
  , UnwrapMaybe typ unwrapped
  ) =>
  FlushWhereWordJQByHead head word currentType tables paramsIn unwrapped paramsIn

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- whereJQ: WHERE for join queries
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

whereJQ
  :: forall @whr tables result params p stage stage'
   . IsSymbol whr
  => ParseWhereJQ whr tables params
  => Row.Lacks "where" stage
  => Row.Lacks "insert" stage
  => Row.Cons "where" Unit stage stage'
  => JQ tables result p stage
  -> JQ tables result params stage'
whereJQ (JQ q) = JQ (q { sql = q.sql <> " WHERE " <> reflectSymbol (Proxy :: Proxy whr) })

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- ValidateOrderByJQ: validate ORDER BY with qualified column support
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class ValidateOrderByJQ :: Symbol -> Row (Row Type) -> Constraint
class ValidateOrderByJQ sym tables

instance ValidateOrderByJQ "" tables
else instance
  ( Symbol.Cons h t sym
  , ValidateOrderByJQGo h t "" tables
  ) =>
  ValidateOrderByJQ sym tables

class ValidateOrderByJQGo :: Symbol -> Symbol -> Symbol -> Row (Row Type) -> Constraint
class ValidateOrderByJQGo head tail acc tables

-- Comma: flush column, continue
instance
  ( FlushOrderByWordJQ acc tables
  , SkipSpaces tail rest
  , ValidateOrderByJQ rest tables
  ) =>
  ValidateOrderByJQGo "," tail acc tables

-- Space: flush column, skip modifiers
else instance
  ( SkipSpaces tail rest
  , FlushOrderByJQThenSkip acc rest tables
  ) =>
  ValidateOrderByJQGo " " tail acc tables

-- End of string: flush final column
else instance
  ( Symbol.Append acc h acc'
  , FlushOrderByWordJQ acc' tables
  ) =>
  ValidateOrderByJQGo h "" acc tables

-- Regular char (including dot): accumulate
else instance
  ( Symbol.Append acc h acc'
  , Symbol.Cons nextH nextT tail
  , ValidateOrderByJQGo nextH nextT acc' tables
  ) =>
  ValidateOrderByJQGo h tail acc tables

class FlushOrderByWordJQ :: Symbol -> Row (Row Type) -> Constraint
class FlushOrderByWordJQ word tables

instance FlushOrderByWordJQ "" tables
else instance FlushOrderByWordJQ "ASC" tables
else instance FlushOrderByWordJQ "asc" tables
else instance FlushOrderByWordJQ "DESC" tables
else instance FlushOrderByWordJQ "desc" tables
else instance FlushOrderByWordJQ "NULLS" tables
else instance FlushOrderByWordJQ "FIRST" tables
else instance FlushOrderByWordJQ "LAST" tables
else instance ResolveColumn word tables typ => FlushOrderByWordJQ word tables

class FlushOrderByJQThenSkip :: Symbol -> Symbol -> Row (Row Type) -> Constraint
class FlushOrderByJQThenSkip colName rest tables

instance FlushOrderByWordJQ colName tables => FlushOrderByJQThenSkip colName "" tables
else instance
  ( FlushOrderByWordJQ colName tables
  , Symbol.Cons h t rest
  , FlushOrderByJQThenSkipByHead h t tables
  ) =>
  FlushOrderByJQThenSkip colName rest tables

class FlushOrderByJQThenSkipByHead :: Symbol -> Symbol -> Row (Row Type) -> Constraint
class FlushOrderByJQThenSkipByHead head tail tables

-- Comma: continue with next column
instance
  ( SkipSpaces tail rest
  , ValidateOrderByJQ rest tables
  ) =>
  FlushOrderByJQThenSkipByHead "," tail tables

-- Modifier word: consume it, continue
else instance
  ( Symbol.Append h t rest
  , ExtractWord rest word afterWord
  , FlushOrderByWordJQ word tables
  , SkipSpaces afterWord rest'
  , ValidateOrderByJQContinue rest' tables
  ) =>
  FlushOrderByJQThenSkipByHead h t tables

class ValidateOrderByJQContinue :: Symbol -> Row (Row Type) -> Constraint
class ValidateOrderByJQContinue sym tables

instance ValidateOrderByJQContinue "" tables
else instance
  ( Symbol.Cons h t sym
  , ValidateOrderByJQContinueByHead h t tables
  ) =>
  ValidateOrderByJQContinue sym tables

class ValidateOrderByJQContinueByHead :: Symbol -> Symbol -> Row (Row Type) -> Constraint
class ValidateOrderByJQContinueByHead head tail tables

instance
  ( SkipSpaces tail rest
  , ValidateOrderByJQ rest tables
  ) =>
  ValidateOrderByJQContinueByHead "," tail tables

else instance
  ( Symbol.Append h t rest
  , ExtractWord rest word afterWord
  , FlushOrderByWordJQ word tables
  , SkipSpaces afterWord rest'
  , ValidateOrderByJQContinue rest' tables
  ) =>
  ValidateOrderByJQContinueByHead h t tables

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- orderByJQ / limitJQ / offsetJQ
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

orderByJQ
  :: forall @cols tables result params stage stage'
   . IsSymbol cols
  => ValidateOrderByJQ cols tables
  => HasClause "select" stage
  => Row.Lacks "orderBy" stage
  => Row.Lacks "limit" stage
  => Row.Lacks "offset" stage
  => Row.Cons "orderBy" Unit stage stage'
  => JQ tables result params stage
  -> JQ tables result params stage'
orderByJQ (JQ q) = JQ (q { sql = q.sql <> " ORDER BY " <> reflectSymbol (Proxy :: Proxy cols) })

limitJQ
  :: forall tables result params stage stage'
   . HasClause "select" stage
  => Row.Lacks "limit" stage
  => Row.Cons "limit" Unit stage stage'
  => Int
  -> JQ tables result params stage
  -> JQ tables result params stage'
limitJQ n (JQ q) = JQ (q { sql = q.sql <> " LIMIT " <> show n })

offsetJQ
  :: forall tables result params stage stage'
   . HasClause "select" stage
  => Row.Lacks "offset" stage
  => Row.Cons "offset" Unit stage stage'
  => Int
  -> JQ tables result params stage
  -> JQ tables result params stage'
offsetJQ n (JQ q) = JQ (q { sql = q.sql <> " OFFSET " <> show n })

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- JQ execution functions
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

runQueryJQ
  :: forall tables result params paramsRL stage
   . RowToList params paramsRL
  => ParamsToArray paramsRL params
  => ReadForeign { | result }
  => PG.Connection
  -> { | params }
  -> JQ tables result params stage
  -> Aff (Array { | result })
runQueryJQ conn params (JQ q) = do
  let entries = paramsToArray (Proxy :: Proxy paramsRL) params
  let { sql, values } = replaceNamedParams (Array.length q.values) entries q.sql
  let allValues = q.values <> values
  result <- PG.query (PG.SQL sql) allValues conn
  pure (unsafeDecodeRows result.rows)

runQueryOneJQ
  :: forall tables result params paramsRL stage
   . RowToList params paramsRL
  => ParamsToArray paramsRL params
  => ReadForeign { | result }
  => PG.Connection
  -> { | params }
  -> JQ tables result params stage
  -> Aff (Maybe { | result })
runQueryOneJQ conn params (JQ q) = do
  let entries = paramsToArray (Proxy :: Proxy paramsRL) params
  let { sql, values } = replaceNamedParams (Array.length q.values) entries q.sql
  let allValues = q.values <> values
  result <- PG.queryOne (PG.SQL sql) allValues conn
  pure (result <#> unsafeDecodeRow)

runExecuteJQ
  :: forall tables params paramsRL stage
   . RowToList params paramsRL
  => ParamsToArray paramsRL params
  => PG.Connection
  -> { | params }
  -> JQ tables () params stage
  -> Aff Int
runExecuteJQ conn params (JQ q) = do
  let entries = paramsToArray (Proxy :: Proxy paramsRL) params
  let { sql, values } = replaceNamedParams (Array.length q.values) entries q.sql
  let allValues = q.values <> values
  PG.execute (PG.SQL sql) allValues conn

-- Transaction variants

runQueryJQTx
  :: forall tables result params paramsRL stage
   . RowToList params paramsRL
  => ParamsToArray paramsRL params
  => ReadForeign { | result }
  => PG.Transaction
  -> { | params }
  -> JQ tables result params stage
  -> Aff (Array { | result })
runQueryJQTx txn params (JQ q) = do
  let entries = paramsToArray (Proxy :: Proxy paramsRL) params
  let { sql, values } = replaceNamedParams (Array.length q.values) entries q.sql
  let allValues = q.values <> values
  result <- PG.txQuery (PG.SQL sql) allValues txn
  pure (unsafeDecodeRows result.rows)

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- leftJoin: Q → JQ with nullable right-table columns
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class MakeNullableRL :: RL.RowList Type -> RL.RowList Type -> Constraint
class MakeNullableRL rl out | rl -> out

instance MakeNullableRL RL.Nil RL.Nil
instance MakeNullableRL tail out' => MakeNullableRL (RL.Cons name (Column (Maybe a) constraints) tail) (RL.Cons name (Column (Maybe a) constraints) out')
else instance MakeNullableRL tail out' => MakeNullableRL (RL.Cons name (Column typ constraints) tail) (RL.Cons name (Column (Maybe typ) constraints) out')

leftJoin
  :: forall @cond name1 cols1 name2 cols2 cols2RL nullableCols2RL nullableCols2 r p tables tables' stage
   . IsSymbol name1
  => IsSymbol name2
  => IsSymbol cond
  => RowToList cols2 cols2RL
  => MakeNullableRL cols2RL nullableCols2RL
  => ListToRow nullableCols2RL nullableCols2
  => Row.Cons name1 cols1 () tables'
  => Row.Cons name2 nullableCols2 tables' tables
  => ValidateJoinCondition cond tables
  => Row.Lacks "select" stage
  => Row.Lacks "insert" stage
  => Row.Lacks "set" stage
  => Row.Lacks "delete" stage
  => Proxy (Table name2 cols2)
  -> Q name1 cols1 r p stage
  -> JQ tables () () (join :: Unit)
leftJoin _ _ = JQ
  { sql: reflectSymbol (Proxy :: Proxy name1)
      <> " LEFT JOIN "
      <> reflectSymbol (Proxy :: Proxy name2)
      <> " ON "
      <> reflectSymbol (Proxy :: Proxy cond)
  , values: []
  }

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Multi-way join chaining: JQ → JQ
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

innerJoinJQ
  :: forall @cond name cols tables tables' r p stage stage'
   . IsSymbol name
  => IsSymbol cond
  => Row.Lacks name tables
  => Row.Cons name cols tables tables'
  => ValidateJoinCondition cond tables'
  => Row.Lacks "select" stage
  => Row.Cons "join" Unit stage stage'
  => Proxy (Table name cols)
  -> JQ tables r p stage
  -> JQ tables' () () stage'
innerJoinJQ _ (JQ q) = JQ
  { sql: q.sql
      <> " INNER JOIN "
      <> reflectSymbol (Proxy :: Proxy name)
      <> " ON "
      <> reflectSymbol (Proxy :: Proxy cond)
  , values: q.values
  }

leftJoinJQ
  :: forall @cond name cols colsRL nullableColsRL nullableCols tables tables' r p stage stage'
   . IsSymbol name
  => IsSymbol cond
  => RowToList cols colsRL
  => MakeNullableRL colsRL nullableColsRL
  => ListToRow nullableColsRL nullableCols
  => Row.Lacks name tables
  => Row.Cons name nullableCols tables tables'
  => ValidateJoinCondition cond tables'
  => Row.Lacks "select" stage
  => Row.Cons "join" Unit stage stage'
  => Proxy (Table name cols)
  -> JQ tables r p stage
  -> JQ tables' () () stage'
leftJoinJQ _ (JQ q) = JQ
  { sql: q.sql
      <> " LEFT JOIN "
      <> reflectSymbol (Proxy :: Proxy name)
      <> " ON "
      <> reflectSymbol (Proxy :: Proxy cond)
  , values: q.values
  }
