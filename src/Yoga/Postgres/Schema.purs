module Yoga.Postgres.Schema where

import Prelude

import Data.Array (intercalate, mapWithIndex)
import Data.Maybe (Maybe)
import Data.Reflectable (class Reflectable, reflectType)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\))
import Prim.Row (class Cons) as Row
import Prim.RowList as RL
import Prim.RowList (class RowToList)
import Prim.Symbol (class Cons, class Append) as Symbol
import Prim.TypeError (class Fail, Beside, Text, Quote)
import Type.Proxy (Proxy(..))
import Type.RowList (class ListToRow)

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
else instance FlushWhereWord "IN" currentType cols paramsIn currentType paramsIn
else instance FlushWhereWord "TRUE" currentType cols paramsIn currentType paramsIn
else instance FlushWhereWord "FALSE" currentType cols paramsIn currentType paramsIn
else instance FlushWhereWord "BETWEEN" currentType cols paramsIn currentType paramsIn
else instance FlushWhereWord "ANY" currentType cols paramsIn currentType paramsIn
else instance FlushWhereWord "ALL" currentType cols paramsIn currentType paramsIn

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

newtype Q :: Symbol -> Row Type -> Row Type -> Row Type -> Type
newtype Q name cols result params = Q String

from :: forall name cols. Proxy (Table name cols) -> Q name cols () ()
from _ = Q ""

selectAll
  :: forall name cols result r p
   . IsSymbol name
  => StripColumns cols result
  => Q name cols r p
  -> Q name cols result p
selectAll _ = Q ("SELECT * FROM " <> reflectSymbol (Proxy :: Proxy name))

select
  :: forall @sel name cols result r p
   . IsSymbol name
  => IsSymbol sel
  => ParseSelect sel cols result
  => Q name cols r p
  -> Q name cols result p
select _ = Q ("SELECT " <> reflectSymbol (Proxy :: Proxy sel) <> " FROM " <> reflectSymbol (Proxy :: Proxy name))

where_
  :: forall @whr name cols result params p
   . IsSymbol whr
  => ParseWhere whr cols params
  => Q name cols result p
  -> Q name cols result params
where_ (Q base) = Q (base <> " WHERE " <> reflectSymbol (Proxy :: Proxy whr))

toSQL :: forall name cols result params. Q name cols result params -> String
toSQL (Q s) = s
