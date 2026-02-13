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
import Type.Function (type (#))
import Effect.Aff (Aff)
import Foreign (Foreign, unsafeToForeign)
import Prim.Boolean (True, False)
import Prim.Row (class Cons, class Lacks, class Union, class Nub) as Row
import Prim.RowList as RL
import Prim.RowList (class RowToList)
import Prim.Symbol (class Cons, class Append) as Symbol
import Prim.TypeError (class Fail, Beside, Text, Quote)
import Record (get) as Record
import Type.Proxy (Proxy(..))
import Type.RowList (class ListToRow)
import Yoga.JSON (class ReadForeign, readImpl)
import Yoga.Postgres as PG

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Core phantom types
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

data Table :: Symbol -> Row Type -> Type
data Table name columns = Table

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Constraint wrappers (used with # operator from Type.Function)
--   Int # PrimaryKey # AutoIncrement = AutoIncrement (PrimaryKey Int)
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

data PrimaryKey :: Type -> Type
data PrimaryKey a

data AutoIncrement :: Type -> Type
data AutoIncrement a

data Unique :: Type -> Type
data Unique a

data Default :: forall k. k -> Type -> Type
data Default val a

data References

data ForeignKey :: Symbol -> Type -> Symbol -> Type -> Type
data ForeignKey table references col a

-- Internal: used by LEFT JOIN to mark columns as nullable
data Nullable :: Type -> Type
data Nullable a

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
-- ExtractType: recursively unwrap constraint wrappers to get base type
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class ExtractType :: Type -> Type -> Constraint
class ExtractType wrapped typ | wrapped -> typ

instance ExtractType a typ => ExtractType (PrimaryKey a) typ
else instance ExtractType a typ => ExtractType (AutoIncrement a) typ
else instance ExtractType a typ => ExtractType (Unique a) typ
else instance ExtractType a typ => ExtractType (Default val a) typ
else instance ExtractType a typ => ExtractType (ForeignKey t r c a) typ
else instance ExtractType a typ => ExtractType (Nullable a) (Maybe typ)
else instance ExtractType a a

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- PureScript type -> Postgres type name
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
-- Constraint -> DDL fragment
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class RenderDefaultValue val typ where
  renderDefaultValue :: Proxy val -> Proxy typ -> String

instance Reflectable sym String => RenderDefaultValue sym String where
  renderDefaultValue _ _ = "DEFAULT '" <> reflectType (Proxy :: Proxy sym) <> "'"

instance Reflectable val Int => RenderDefaultValue val Int where
  renderDefaultValue _ _ = "DEFAULT " <> show (reflectType (Proxy :: Proxy val))

instance Reflectable val Boolean => RenderDefaultValue val Boolean where
  renderDefaultValue _ _ = if reflectType (Proxy :: Proxy val) then "DEFAULT true" else "DEFAULT false"

class RenderConstraint a where
  renderConstraint :: Proxy a -> String

instance RenderConstraint a => RenderConstraint (PrimaryKey a) where
  renderConstraint _ = joinConstraints "PRIMARY KEY" (renderConstraint (Proxy :: Proxy a))

else instance RenderConstraint a => RenderConstraint (AutoIncrement a) where
  renderConstraint _ = joinConstraints "GENERATED ALWAYS AS IDENTITY" (renderConstraint (Proxy :: Proxy a))

else instance RenderConstraint a => RenderConstraint (Unique a) where
  renderConstraint _ = joinConstraints "UNIQUE" (renderConstraint (Proxy :: Proxy a))

else instance (RenderDefaultValue val a, RenderConstraint a) => RenderConstraint (Default val a) where
  renderConstraint _ = joinConstraints (renderDefaultValue (Proxy :: Proxy val) (Proxy :: Proxy a)) (renderConstraint (Proxy :: Proxy a))

else instance (IsSymbol table, IsSymbol col, RenderConstraint a) => RenderConstraint (ForeignKey table References col a) where
  renderConstraint _ = joinConstraints
    ("REFERENCES " <> reflectSymbol (Proxy :: Proxy table) <> "(" <> reflectSymbol (Proxy :: Proxy col) <> ")")
    (renderConstraint (Proxy :: Proxy a))

else instance RenderConstraint a => RenderConstraint (Nullable a) where
  renderConstraint _ = renderConstraint (Proxy :: Proxy a)

else instance RenderConstraint a where
  renderConstraint _ = ""

joinConstraints :: String -> String -> String
joinConstraints a b = case a, b of
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
  , ExtractType entry typ
  , PGTypeName typ
  , IsNullable typ
  , RenderConstraint entry
  , RenderColumnsRL tail
  ) =>
  RenderColumnsRL (RL.Cons name entry tail) where
  renderColumnsRL _ = do
    let colName = reflectSymbol (Proxy :: Proxy name)
    let colType = pgTypeName (Proxy :: Proxy typ)
    let nullable = isNullable (Proxy :: Proxy typ)
    let notNull = if nullable then "" else " NOT NULL"
    let constraints = renderConstraint (Proxy :: Proxy entry)
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

instance IsAutoGenerated (AutoIncrement a) where
  isAutoGenerated _ = true
else instance IsAutoGenerated (Default v a) where
  isAutoGenerated _ = true
else instance IsAutoGenerated a => IsAutoGenerated (PrimaryKey a) where
  isAutoGenerated _ = isAutoGenerated (Proxy :: Proxy a)
else instance IsAutoGenerated a => IsAutoGenerated (Unique a) where
  isAutoGenerated _ = isAutoGenerated (Proxy :: Proxy a)
else instance IsAutoGenerated a => IsAutoGenerated (ForeignKey t r c a) where
  isAutoGenerated _ = isAutoGenerated (Proxy :: Proxy a)
else instance IsAutoGenerated a => IsAutoGenerated (Nullable a) where
  isAutoGenerated _ = isAutoGenerated (Proxy :: Proxy a)
else instance IsAutoGenerated a where
  isAutoGenerated _ = false

class InsertColumnsRL :: RL.RowList Type -> Constraint
class InsertColumnsRL rl where
  insertColumnsRL :: Proxy rl -> Array String

instance InsertColumnsRL RL.Nil where
  insertColumnsRL _ = []

instance
  ( IsSymbol name
  , IsAutoGenerated entry
  , InsertColumnsRL tail
  ) =>
  InsertColumnsRL (RL.Cons name entry tail) where
  insertColumnsRL _ =
    let
      rest = insertColumnsRL (Proxy :: Proxy tail)
    in
      if isAutoGenerated (Proxy :: Proxy entry) then rest
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
-- Utility type classes
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class UnwrapMaybe :: Type -> Type -> Constraint
class UnwrapMaybe a b | a -> b

instance UnwrapMaybe (Maybe a) a
else instance UnwrapMaybe a a

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

-- StripColumns: (name :: String # Unique, ...) -> (name :: String, ...)
class StripColumnsRL :: RL.RowList Type -> RL.RowList Type -> Constraint
class StripColumnsRL rl out | rl -> out

instance StripColumnsRL RL.Nil RL.Nil
instance (ExtractType entry typ, StripColumnsRL tail out') => StripColumnsRL (RL.Cons name entry tail) (RL.Cons name typ out')

class StripColumns :: Row Type -> Row Type -> Constraint
class StripColumns cols result | cols -> result

instance (RowToList cols rl, StripColumnsRL rl outRL, ListToRow outRL result) => StripColumns cols result

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- SingleTable: extract name and cols from a single-entry tables row
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class SingleTable :: Row (Row Type) -> Symbol -> Row Type -> Constraint
class SingleTable tables name cols | tables -> name cols

instance
  ( RowToList tables (RL.Cons name cols RL.Nil)
  , IsSymbol name
  ) =>
  SingleTable tables name cols

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- SplitOnDot: "users.id" -> ("users", "id"); "name" -> unqualified
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
  ( Fail (Beside (Text "Column ") (Beside (Quote col) (Text " not found in any table")))
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
-- ParseSelect: parse "users.name, posts.title AS t" against tables
-- Result labels: column name (after dot) or explicit AS alias
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class ParseSelect :: Symbol -> Row (Row Type) -> Row Type -> Constraint
class ParseSelect sym tables result | sym tables -> result

instance ParseSelect "" tables ()
else instance
  ( Symbol.Cons h t sym
  , ParseSelectGo h t "" tables RL.Nil outRL
  , ListToRow outRL result
  ) =>
  ParseSelect sym tables result

class ParseSelectGo :: Symbol -> Symbol -> Symbol -> Row (Row Type) -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseSelectGo head tail acc tables accRL outRL | head tail acc tables accRL -> outRL

-- Leading/double comma: no column name before comma
instance
  Fail (Text "Unexpected comma in SELECT clause (missing column name)") =>
  ParseSelectGo "," tail "" tables accRL outRL

-- Comma: emit column, continue
else instance
  ( ResolveColumn acc tables entry
  , ExtractType entry typ
  , SplitOnDot acc _hasDot _table colName
  , SkipSpaces tail rest
  , ParseSelectContinue rest tables (RL.Cons colName typ accRL) outRL
  ) =>
  ParseSelectGo "," tail acc tables accRL outRL

-- Space: column reference done, check for AS or comma
else instance
  ( SkipSpaces tail rest
  , ParseSelectAfterCol acc rest tables accRL outRL
  ) =>
  ParseSelectGo " " tail acc tables accRL outRL

-- Open paren: aggregate function call
else instance
  ( ExtractUntilParen tail args afterParen
  , ResolveAggregateArg args tables argType
  , AggregateReturnType acc argType returnType
  , SkipSpaces afterParen rest
  , ParseAfterAggregate rest tables returnType accRL outRL
  ) =>
  ParseSelectGo "(" tail acc tables accRL outRL

-- End of string: emit final column
else instance
  ( Symbol.Append acc h acc'
  , ResolveColumn acc' tables entry
  , ExtractType entry typ
  , SplitOnDot acc' _hasDot _table colName
  ) =>
  ParseSelectGo h "" acc tables accRL (RL.Cons colName typ accRL)

-- Regular char (including dot): accumulate
else instance
  ( Symbol.Append acc h acc'
  , Symbol.Cons nextH nextT tail
  , ParseSelectGo nextH nextT acc' tables accRL outRL
  ) =>
  ParseSelectGo h tail acc tables accRL outRL

-- After column name + space: AS alias, comma, or end
class ParseSelectAfterCol :: Symbol -> Symbol -> Row (Row Type) -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseSelectAfterCol colRef rest tables accRL outRL | colRef rest tables accRL -> outRL

-- End: emit column with default label (column name after dot)
instance
  ( ResolveColumn colRef tables entry
  , ExtractType entry typ
  , SplitOnDot colRef _hasDot _table colName
  ) =>
  ParseSelectAfterCol colRef "" tables accRL (RL.Cons colName typ accRL)

-- Non-empty: branch on first char
else instance
  ( Symbol.Cons h t rest
  , ParseSelectAfterColByHead h t colRef tables accRL outRL
  ) =>
  ParseSelectAfterCol colRef rest tables accRL outRL

class ParseSelectAfterColByHead :: Symbol -> Symbol -> Symbol -> Row (Row Type) -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseSelectAfterColByHead head tail colRef tables accRL outRL | head tail colRef tables accRL -> outRL

-- Comma: emit column with default label, continue
instance
  ( ResolveColumn colRef tables entry
  , ExtractType entry typ
  , SplitOnDot colRef _hasDot _table colName
  , SkipSpaces tail rest
  , ParseSelectContinue rest tables (RL.Cons colName typ accRL) outRL
  ) =>
  ParseSelectAfterColByHead "," tail colRef tables accRL outRL

-- Otherwise (AS ...): extract word
else instance
  ( Symbol.Append h t rest
  , ExtractWord rest keyword afterKeyword
  , ParseSelectHandleAS keyword afterKeyword colRef tables accRL outRL
  ) =>
  ParseSelectAfterColByHead h t colRef tables accRL outRL

class ParseSelectHandleAS :: Symbol -> Symbol -> Symbol -> Row (Row Type) -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseSelectHandleAS keyword afterKeyword colRef tables accRL outRL | keyword afterKeyword colRef tables accRL -> outRL

instance
  ( ExtractWord afterKeyword alias afterAlias
  , ResolveColumn colRef tables entry
  , ExtractType entry typ
  , SkipSpaces afterAlias rest
  , ParseSelectExpectEnd rest tables (RL.Cons alias typ accRL) outRL
  ) =>
  ParseSelectHandleAS "AS" afterKeyword colRef tables accRL outRL

else instance
  ( ExtractWord afterKeyword alias afterAlias
  , ResolveColumn colRef tables entry
  , ExtractType entry typ
  , SkipSpaces afterAlias rest
  , ParseSelectExpectEnd rest tables (RL.Cons alias typ accRL) outRL
  ) =>
  ParseSelectHandleAS "as" afterKeyword colRef tables accRL outRL

class ParseSelectExpectEnd :: Symbol -> Row (Row Type) -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseSelectExpectEnd sym tables accRL outRL | sym tables accRL -> outRL

instance ParseSelectExpectEnd "" tables accRL accRL
else instance
  ( Symbol.Cons h t sym
  , ParseSelectExpectEndByHead h t tables accRL outRL
  ) =>
  ParseSelectExpectEnd sym tables accRL outRL

class ParseSelectExpectEndByHead :: Symbol -> Symbol -> Row (Row Type) -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseSelectExpectEndByHead head tail tables accRL outRL | head tail tables accRL -> outRL

instance
  ( SkipSpaces tail rest
  , ParseSelectContinue rest tables accRL outRL
  ) =>
  ParseSelectExpectEndByHead "," tail tables accRL outRL

class ParseSelectContinue :: Symbol -> Row (Row Type) -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseSelectContinue sym tables accRL outRL | sym tables accRL -> outRL

instance Fail (Text "Trailing comma in SELECT clause") => ParseSelectContinue "" tables accRL outRL
else instance
  ( Symbol.Cons h t sym
  , ParseSelectGo h t "" tables accRL outRL
  ) =>
  ParseSelectContinue sym tables accRL outRL

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Aggregate functions in SELECT: COUNT(*), SUM(col), etc.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- Extract characters until closing paren
class ExtractUntilParen :: Symbol -> Symbol -> Symbol -> Constraint
class ExtractUntilParen tail args afterParen | tail -> args afterParen

instance
  Fail (Text "Unclosed parenthesis in SELECT clause") =>
  ExtractUntilParen "" args afterParen

else instance
  ( Symbol.Cons h t tail
  , ExtractUntilParenGo h t "" args afterParen
  ) =>
  ExtractUntilParen tail args afterParen

class ExtractUntilParenGo :: Symbol -> Symbol -> Symbol -> Symbol -> Symbol -> Constraint
class ExtractUntilParenGo head tail acc args afterParen | head tail acc -> args afterParen

instance ExtractUntilParenGo ")" tail acc acc tail

else instance
  Fail (Text "Unclosed parenthesis in SELECT clause") =>
  ExtractUntilParenGo h "" acc args afterParen

else instance
  ( Symbol.Append acc h acc'
  , Symbol.Cons nextH nextT tail
  , ExtractUntilParenGo nextH nextT acc' args afterParen
  ) =>
  ExtractUntilParenGo h tail acc args afterParen

-- Resolve aggregate argument: "*" -> Star, column ref -> unwrapped type
data Star

class ResolveAggregateArg :: Symbol -> Row (Row Type) -> Type -> Constraint
class ResolveAggregateArg args tables argType | args tables -> argType

instance ResolveAggregateArg "*" tables Star
else instance ResolveAggregateArg "" tables Star
else instance
  ( SkipSpaces args trimmedFront
  , ExtractWord trimmedFront col _rest
  , ResolveAggregateArgCol col tables argType
  ) =>
  ResolveAggregateArg args tables argType

class ResolveAggregateArgCol :: Symbol -> Row (Row Type) -> Type -> Constraint
class ResolveAggregateArgCol col tables argType | col tables -> argType

instance ResolveAggregateArgCol "*" tables Star
else instance
  ( Symbol.Cons h _t col
  , ResolveAggregateArgColByHead h col tables argType
  ) =>
  ResolveAggregateArgCol col tables argType

class ResolveAggregateArgColByHead :: Symbol -> Symbol -> Row (Row Type) -> Type -> Constraint
class ResolveAggregateArgColByHead head col tables argType | head col tables -> argType

-- Numeric literals: treat as Star (return type determined by function)
instance ResolveAggregateArgColByHead "0" col tables Star
else instance ResolveAggregateArgColByHead "1" col tables Star
else instance ResolveAggregateArgColByHead "2" col tables Star
else instance ResolveAggregateArgColByHead "3" col tables Star
else instance ResolveAggregateArgColByHead "4" col tables Star
else instance ResolveAggregateArgColByHead "5" col tables Star
else instance ResolveAggregateArgColByHead "6" col tables Star
else instance ResolveAggregateArgColByHead "7" col tables Star
else instance ResolveAggregateArgColByHead "8" col tables Star
else instance ResolveAggregateArgColByHead "9" col tables Star
-- Column reference
else instance
  ( ResolveColumn col tables entry
  , ExtractType entry typ
  , UnwrapMaybe typ unwrapped
  ) =>
  ResolveAggregateArgColByHead head col tables unwrapped

-- Map (funcName, argType) -> returnType
class AggregateReturnType :: Symbol -> Type -> Type -> Constraint
class AggregateReturnType funcName argType returnType | funcName argType -> returnType

instance AggregateReturnType "COUNT" argType Int
else instance AggregateReturnType "count" argType Int
else instance AggregateReturnType "SUM" argType argType
else instance AggregateReturnType "sum" argType argType
else instance AggregateReturnType "AVG" argType Number
else instance AggregateReturnType "avg" argType Number
else instance AggregateReturnType "MIN" argType argType
else instance AggregateReturnType "min" argType argType
else instance AggregateReturnType "MAX" argType argType
else instance AggregateReturnType "max" argType argType
else instance AggregateReturnType "ARRAY_AGG" argType (Array argType)
else instance AggregateReturnType "array_agg" argType (Array argType)
else instance AggregateReturnType "STRING_AGG" argType String
else instance AggregateReturnType "string_agg" argType String
else instance AggregateReturnType "COALESCE" argType argType
else instance AggregateReturnType "coalesce" argType argType
-- Window functions
else instance AggregateReturnType "ROW_NUMBER" argType Int
else instance AggregateReturnType "row_number" argType Int
else instance AggregateReturnType "RANK" argType Int
else instance AggregateReturnType "rank" argType Int
else instance AggregateReturnType "DENSE_RANK" argType Int
else instance AggregateReturnType "dense_rank" argType Int
else instance AggregateReturnType "NTILE" argType Int
else instance AggregateReturnType "ntile" argType Int
else instance AggregateReturnType "LAG" argType argType
else instance AggregateReturnType "lag" argType argType
else instance AggregateReturnType "LEAD" argType argType
else instance AggregateReturnType "lead" argType argType
else instance AggregateReturnType "FIRST_VALUE" argType argType
else instance AggregateReturnType "first_value" argType argType
else instance AggregateReturnType "LAST_VALUE" argType argType
else instance AggregateReturnType "last_value" argType argType
else instance AggregateReturnType "NTH_VALUE" argType argType
else instance AggregateReturnType "nth_value" argType argType
else instance
  Fail (Beside (Text "Unknown function: ") (Quote funcName)) =>
  AggregateReturnType funcName argType returnType

-- After aggregate ): require AS alias, then continue
class ParseAfterAggregate :: Symbol -> Row (Row Type) -> Type -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseAfterAggregate rest tables returnType accRL outRL | rest tables returnType accRL -> outRL

-- End of string without alias
instance
  Fail (Text "Aggregate function requires AS alias (e.g. COUNT(*) AS cnt)") =>
  ParseAfterAggregate "" tables returnType accRL outRL

else instance
  ( Symbol.Cons h t rest
  , ParseAfterAggregateByHead h t tables returnType accRL outRL
  ) =>
  ParseAfterAggregate rest tables returnType accRL outRL

class ParseAfterAggregateByHead :: Symbol -> Symbol -> Row (Row Type) -> Type -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseAfterAggregateByHead head tail tables returnType accRL outRL | head tail tables returnType accRL -> outRL

-- Comma without alias
instance
  Fail (Text "Aggregate function requires AS alias (e.g. COUNT(*) AS cnt)") =>
  ParseAfterAggregateByHead "," tail tables returnType accRL outRL

-- Otherwise: extract keyword and dispatch (OVER or AS)
else instance
  ( Symbol.Append h t rest
  , ExtractWord rest keyword afterKeyword
  , ParseAfterAggregateKeyword keyword afterKeyword tables returnType accRL outRL
  ) =>
  ParseAfterAggregateByHead h t tables returnType accRL outRL

class ParseAfterAggregateKeyword :: Symbol -> Symbol -> Row (Row Type) -> Type -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseAfterAggregateKeyword keyword afterKeyword tables returnType accRL outRL | keyword afterKeyword tables returnType accRL -> outRL

-- OVER: parse the over clause, then continue to AS
instance
  ( SkipSpaces afterKeyword rest
  , ParseOverClause rest tables returnType accRL outRL
  ) =>
  ParseAfterAggregateKeyword "OVER" afterKeyword tables returnType accRL outRL

else instance
  ( SkipSpaces afterKeyword rest
  , ParseOverClause rest tables returnType accRL outRL
  ) =>
  ParseAfterAggregateKeyword "over" afterKeyword tables returnType accRL outRL

-- AS: extract alias
else instance
  ( ExtractWord afterKeyword alias afterAlias
  , SkipSpaces afterAlias rest
  , ParseSelectExpectEnd rest tables (RL.Cons alias returnType accRL) outRL
  ) =>
  ParseAfterAggregateKeyword "AS" afterKeyword tables returnType accRL outRL

else instance
  ( ExtractWord afterKeyword alias afterAlias
  , SkipSpaces afterAlias rest
  , ParseSelectExpectEnd rest tables (RL.Cons alias returnType accRL) outRL
  ) =>
  ParseAfterAggregateKeyword "as" afterKeyword tables returnType accRL outRL

else instance
  Fail (Text "Aggregate function requires AS alias (e.g. COUNT(*) AS cnt)") =>
  ParseAfterAggregateKeyword keyword afterKeyword tables returnType accRL outRL

-- Parse OVER (...) clause: expect (, skip until ), then continue to AS alias
class ParseOverClause :: Symbol -> Row (Row Type) -> Type -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseOverClause rest tables returnType accRL outRL | rest tables returnType accRL -> outRL

instance
  Fail (Text "Expected ( after OVER") =>
  ParseOverClause "" tables returnType accRL outRL

else instance
  ( Symbol.Cons h t rest
  , ParseOverClauseByHead h t tables returnType accRL outRL
  ) =>
  ParseOverClause rest tables returnType accRL outRL

class ParseOverClauseByHead :: Symbol -> Symbol -> Row (Row Type) -> Type -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseOverClauseByHead head tail tables returnType accRL outRL | head tail tables returnType accRL -> outRL

-- Open paren: extract until close paren, skip content, continue to AS
instance
  ( ExtractUntilParen tail _overContent afterParen
  , SkipSpaces afterParen rest
  , ParseAfterAggregate rest tables returnType accRL outRL
  ) =>
  ParseOverClauseByHead "(" tail tables returnType accRL outRL

else instance
  Fail (Text "Expected ( after OVER") =>
  ParseOverClauseByHead h t tables returnType accRL outRL

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- ParseWhere: parse "id = $id AND age > $age" -> params
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

data NoType

class ParseWhere :: Symbol -> Row (Row Type) -> Row Type -> Constraint
class ParseWhere sym tables params | sym tables -> params

instance ParseWhere "" tables ()
else instance
  ( Symbol.Cons h t sym
  , ParseWhereGo h t "" NoType tables RL.Nil outRL
  , ListToRow outRL params
  ) =>
  ParseWhere sym tables params

class ParseWhereGo :: Symbol -> Symbol -> Symbol -> Type -> Row (Row Type) -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseWhereGo head tail acc currentType tables paramsIn paramsOut | head tail acc currentType tables paramsIn -> paramsOut

-- Space: flush word, continue
instance
  ( FlushWhereWord acc currentType tables paramsIn currentType' paramsOut'
  , SkipSpaces tail rest
  , ParseWhereContinue rest currentType' tables paramsOut' paramsOut
  ) =>
  ParseWhereGo " " tail acc currentType tables paramsIn paramsOut

-- Operators
else instance (FlushWhereWord acc currentType tables paramsIn currentType' paramsOut', ParseWhereContinue tail currentType' tables paramsOut' paramsOut) => ParseWhereGo "=" tail acc currentType tables paramsIn paramsOut
else instance (FlushWhereWord acc currentType tables paramsIn currentType' paramsOut', ParseWhereContinue tail currentType' tables paramsOut' paramsOut) => ParseWhereGo ">" tail acc currentType tables paramsIn paramsOut
else instance (FlushWhereWord acc currentType tables paramsIn currentType' paramsOut', ParseWhereContinue tail currentType' tables paramsOut' paramsOut) => ParseWhereGo "<" tail acc currentType tables paramsIn paramsOut
else instance (FlushWhereWord acc currentType tables paramsIn currentType' paramsOut', ParseWhereContinue tail currentType' tables paramsOut' paramsOut) => ParseWhereGo "!" tail acc currentType tables paramsIn paramsOut
else instance (FlushWhereWord acc currentType tables paramsIn currentType' paramsOut', ParseWhereContinue tail currentType' tables paramsOut' paramsOut) => ParseWhereGo "(" tail acc currentType tables paramsIn paramsOut
else instance (FlushWhereWord acc currentType tables paramsIn currentType' paramsOut', ParseWhereContinue tail currentType' tables paramsOut' paramsOut) => ParseWhereGo ")" tail acc currentType tables paramsIn paramsOut
else instance (FlushWhereWord acc currentType tables paramsIn currentType' paramsOut', ParseWhereContinue tail currentType' tables paramsOut' paramsOut) => ParseWhereGo "'" tail acc currentType tables paramsIn paramsOut
else instance (FlushWhereWord acc currentType tables paramsIn currentType' paramsOut', ParseWhereContinue tail currentType' tables paramsOut' paramsOut) => ParseWhereGo "@" tail acc currentType tables paramsIn paramsOut
else instance (FlushWhereWord acc currentType tables paramsIn currentType' paramsOut', ParseWhereContinue tail currentType' tables paramsOut' paramsOut) => ParseWhereGo "?" tail acc currentType tables paramsIn paramsOut
else instance (FlushWhereWord acc currentType tables paramsIn currentType' paramsOut', ParseWhereContinue tail currentType' tables paramsOut' paramsOut) => ParseWhereGo ":" tail acc currentType tables paramsIn paramsOut
else instance (FlushWhereWord acc currentType tables paramsIn currentType' paramsOut', ParseWhereContinue tail currentType' tables paramsOut' paramsOut) => ParseWhereGo "~" tail acc currentType tables paramsIn paramsOut
else instance (FlushWhereWord acc currentType tables paramsIn currentType' paramsOut', ParseWhereContinue tail currentType' tables paramsOut' paramsOut) => ParseWhereGo "#" tail acc currentType tables paramsIn paramsOut

-- End of string: flush final word
else instance
  ( Symbol.Append acc h acc'
  , FlushWhereWord acc' currentType tables paramsIn _ct paramsOut
  ) =>
  ParseWhereGo h "" acc currentType tables paramsIn paramsOut

-- Regular char (including dot): accumulate
else instance
  ( Symbol.Append acc h acc'
  , Symbol.Cons nextH nextT tail
  , ParseWhereGo nextH nextT acc' currentType tables paramsIn paramsOut
  ) =>
  ParseWhereGo h tail acc currentType tables paramsIn paramsOut

class ParseWhereContinue :: Symbol -> Type -> Row (Row Type) -> RL.RowList Type -> RL.RowList Type -> Constraint
class ParseWhereContinue sym currentType tables paramsIn paramsOut | sym currentType tables paramsIn -> paramsOut

instance ParseWhereContinue "" currentType tables paramsIn paramsIn
else instance
  ( Symbol.Cons h t sym
  , ParseWhereGo h t "" currentType tables paramsIn paramsOut
  ) =>
  ParseWhereContinue sym currentType tables paramsIn paramsOut

-- Flush a word in WHERE context
class FlushWhereWord :: Symbol -> Type -> Row (Row Type) -> RL.RowList Type -> Type -> RL.RowList Type -> Constraint
class FlushWhereWord word currentType tables paramsIn currentTypeOut paramsOut | word currentType tables paramsIn -> currentTypeOut paramsOut

instance FlushWhereWord "" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "AND" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "OR" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "NOT" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "IS" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "NULL" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "LIKE" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "ILIKE" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "IN" currentType tables paramsIn (Array currentType) paramsIn
else instance FlushWhereWord "TRUE" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "FALSE" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "BETWEEN" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "ANY" currentType tables paramsIn (Array currentType) paramsIn
else instance FlushWhereWord "ALL" currentType tables paramsIn (Array currentType) paramsIn
else instance FlushWhereWord "CAST" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "AS" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "EXISTS" currentType tables paramsIn currentType paramsIn
-- Lowercase SQL keywords
else instance FlushWhereWord "and" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "or" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "not" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "is" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "null" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "like" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "ilike" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "in" currentType tables paramsIn (Array currentType) paramsIn
else instance FlushWhereWord "true" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "false" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "between" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "any" currentType tables paramsIn (Array currentType) paramsIn
else instance FlushWhereWord "all" currentType tables paramsIn (Array currentType) paramsIn
else instance FlushWhereWord "cast" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "as" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "exists" currentType tables paramsIn currentType paramsIn
-- Aggregate functions (for HAVING support)
else instance FlushWhereWord "COUNT" currentType tables paramsIn Int paramsIn
else instance FlushWhereWord "SUM" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "AVG" currentType tables paramsIn Number paramsIn
else instance FlushWhereWord "MIN" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "MAX" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "ARRAY_AGG" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "STRING_AGG" currentType tables paramsIn String paramsIn
-- Lowercase aggregate functions
else instance FlushWhereWord "count" currentType tables paramsIn Int paramsIn
else instance FlushWhereWord "sum" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "avg" currentType tables paramsIn Number paramsIn
else instance FlushWhereWord "min" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "max" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "array_agg" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "string_agg" currentType tables paramsIn String paramsIn
else instance FlushWhereWord "*" currentType tables paramsIn currentType paramsIn
-- Postgres type names (for ::type casts)
else instance FlushWhereWord "text" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "integer" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "bigint" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "boolean" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "jsonb" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "json" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "timestamptz" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "timestamp" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "date" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "int" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "varchar" currentType tables paramsIn currentType paramsIn
else instance FlushWhereWord "uuid" currentType tables paramsIn currentType paramsIn
-- Non-keyword: check first char
else instance
  ( Symbol.Cons head rest word
  , FlushWhereWordByHead head word currentType tables paramsIn currentTypeOut paramsOut
  ) =>
  FlushWhereWord word currentType tables paramsIn currentTypeOut paramsOut

class FlushWhereWordByHead :: Symbol -> Symbol -> Type -> Row (Row Type) -> RL.RowList Type -> Type -> RL.RowList Type -> Constraint
class FlushWhereWordByHead head word currentType tables paramsIn currentTypeOut paramsOut | head word currentType tables paramsIn -> currentTypeOut paramsOut

-- $param: emit with currentType
instance
  ( Symbol.Cons "$" paramName word
  ) =>
  FlushWhereWordByHead "$" word currentType tables paramsIn currentType (RL.Cons paramName currentType paramsIn)

-- Digit: number literal, pass through
else instance FlushWhereWordByHead "0" word currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordByHead "1" word currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordByHead "2" word currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordByHead "3" word currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordByHead "4" word currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordByHead "5" word currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordByHead "6" word currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordByHead "7" word currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordByHead "8" word currentType tables paramsIn currentType paramsIn
else instance FlushWhereWordByHead "9" word currentType tables paramsIn currentType paramsIn

-- Column reference: resolve and set as currentType
else instance
  ( ResolveColumn word tables entry
  , ExtractType entry typ
  , UnwrapMaybe typ unwrapped
  ) =>
  FlushWhereWordByHead head word currentType tables paramsIn unwrapped paramsIn

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- ValidateColumnList: comma-separated column references
-- Used by GROUP BY, DISTINCT ON, ON CONFLICT target
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class ValidateColumnList :: Symbol -> Row (Row Type) -> Constraint
class ValidateColumnList sym tables

instance Fail (Text "Empty column list") => ValidateColumnList "" tables
else instance
  ( Symbol.Cons h t sym
  , ValidateColumnListGo h t "" tables
  ) =>
  ValidateColumnList sym tables

class ValidateColumnListContinue :: Symbol -> Row (Row Type) -> Constraint
class ValidateColumnListContinue sym tables

instance ValidateColumnListContinue "" tables
else instance
  ( Symbol.Cons h t sym
  , ValidateColumnListGo h t "" tables
  ) =>
  ValidateColumnListContinue sym tables

class ValidateColumnListGo :: Symbol -> Symbol -> Symbol -> Row (Row Type) -> Constraint
class ValidateColumnListGo head tail acc tables

instance
  ( ResolveColumn acc tables typ
  , SkipSpaces tail rest
  , ValidateColumnListContinue rest tables
  ) =>
  ValidateColumnListGo "," tail acc tables

else instance
  ( SkipSpaces tail rest
  , ResolveColumn acc tables typ
  , ValidateColumnListContinue rest tables
  ) =>
  ValidateColumnListGo " " tail acc tables

else instance
  ( Symbol.Append acc h acc'
  , ResolveColumn acc' tables typ
  ) =>
  ValidateColumnListGo h "" acc tables

else instance
  ( Symbol.Append acc h acc'
  , Symbol.Cons nextH nextT tail
  , ValidateColumnListGo nextH nextT acc' tables
  ) =>
  ValidateColumnListGo h tail acc tables

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- ValidateOrderBy: validate ORDER BY with qualified column support
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class ValidateOrderBy :: Symbol -> Row (Row Type) -> Constraint
class ValidateOrderBy sym tables

instance ValidateOrderBy "" tables
else instance
  ( Symbol.Cons h t sym
  , ValidateOrderByGo h t "" tables
  ) =>
  ValidateOrderBy sym tables

class ValidateOrderByGo :: Symbol -> Symbol -> Symbol -> Row (Row Type) -> Constraint
class ValidateOrderByGo head tail acc tables

-- Comma: flush column, continue
instance
  ( FlushOrderByWord acc tables
  , SkipSpaces tail rest
  , ValidateOrderBy rest tables
  ) =>
  ValidateOrderByGo "," tail acc tables

-- Space: flush column, skip modifiers
else instance
  ( SkipSpaces tail rest
  , FlushOrderByThenSkip acc rest tables
  ) =>
  ValidateOrderByGo " " tail acc tables

-- End of string: flush final column
else instance
  ( Symbol.Append acc h acc'
  , FlushOrderByWord acc' tables
  ) =>
  ValidateOrderByGo h "" acc tables

-- Regular char (including dot): accumulate
else instance
  ( Symbol.Append acc h acc'
  , Symbol.Cons nextH nextT tail
  , ValidateOrderByGo nextH nextT acc' tables
  ) =>
  ValidateOrderByGo h tail acc tables

class FlushOrderByWord :: Symbol -> Row (Row Type) -> Constraint
class FlushOrderByWord word tables

instance FlushOrderByWord "" tables
else instance FlushOrderByWord "ASC" tables
else instance FlushOrderByWord "asc" tables
else instance FlushOrderByWord "DESC" tables
else instance FlushOrderByWord "desc" tables
else instance FlushOrderByWord "NULLS" tables
else instance FlushOrderByWord "FIRST" tables
else instance FlushOrderByWord "LAST" tables
else instance ResolveColumn word tables typ => FlushOrderByWord word tables

class FlushOrderByThenSkip :: Symbol -> Symbol -> Row (Row Type) -> Constraint
class FlushOrderByThenSkip colName rest tables

instance FlushOrderByWord colName tables => FlushOrderByThenSkip colName "" tables
else instance
  ( FlushOrderByWord colName tables
  , Symbol.Cons h t rest
  , FlushOrderByThenSkipByHead h t tables
  ) =>
  FlushOrderByThenSkip colName rest tables

class FlushOrderByThenSkipByHead :: Symbol -> Symbol -> Row (Row Type) -> Constraint
class FlushOrderByThenSkipByHead head tail tables

-- Comma: continue with next column
instance
  ( SkipSpaces tail rest
  , ValidateOrderBy rest tables
  ) =>
  FlushOrderByThenSkipByHead "," tail tables

-- Modifier word: consume it, continue
else instance
  ( Symbol.Append h t rest
  , ExtractWord rest word afterWord
  , FlushOrderByWord word tables
  , SkipSpaces afterWord rest'
  , ValidateOrderByContinue rest' tables
  ) =>
  FlushOrderByThenSkipByHead h t tables

class ValidateOrderByContinue :: Symbol -> Row (Row Type) -> Constraint
class ValidateOrderByContinue sym tables

instance ValidateOrderByContinue "" tables
else instance
  ( Symbol.Cons h t sym
  , ValidateOrderByContinueByHead h t tables
  ) =>
  ValidateOrderByContinue sym tables

class ValidateOrderByContinueByHead :: Symbol -> Symbol -> Row (Row Type) -> Constraint
class ValidateOrderByContinueByHead head tail tables

instance
  ( SkipSpaces tail rest
  , ValidateOrderBy rest tables
  ) =>
  ValidateOrderByContinueByHead "," tail tables

else instance
  ( Symbol.Append h t rest
  , ExtractWord rest word afterWord
  , FlushOrderByWord word tables
  , SkipSpaces afterWord rest'
  , ValidateOrderByContinue rest' tables
  ) =>
  ValidateOrderByContinueByHead h t tables

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- ValidateColumns: comma-separated column names (for ON CONFLICT target)
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

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
  ( FlushColumnWord acc cols
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
  , FlushColumnWord acc' cols
  ) =>
  ValidateColumnsGo h "" acc cols

-- Regular char: accumulate
else instance
  ( Symbol.Append acc h acc'
  , Symbol.Cons nextH nextT tail
  , ValidateColumnsGo nextH nextT acc' cols
  ) =>
  ValidateColumnsGo h tail acc cols

class FlushColumnWord :: Symbol -> Row Type -> Constraint
class FlushColumnWord word cols

instance FlushColumnWord "" cols
else instance FlushColumnWord "$" cols
else instance FlushColumnWord "AND" cols
else instance FlushColumnWord "OR" cols
else instance FlushColumnWord "NOT" cols
else instance FlushColumnWord "IS" cols
else instance FlushColumnWord "NULL" cols
else instance FlushColumnWord "LIKE" cols
else instance FlushColumnWord "ILIKE" cols
else instance FlushColumnWord "IN" cols
else instance FlushColumnWord "TRUE" cols
else instance FlushColumnWord "FALSE" cols
else instance FlushColumnWord "BETWEEN" cols
else instance FlushColumnWord "ANY" cols
else instance FlushColumnWord "ALL" cols
else instance
  ( Symbol.Cons head rest word
  , FlushColumnWordByHead head word cols
  ) =>
  FlushColumnWord word cols

class FlushColumnWordByHead :: Symbol -> Symbol -> Row Type -> Constraint
class FlushColumnWordByHead head word cols

instance FlushColumnWordByHead "$" word cols
else instance FlushColumnWordByHead "0" word cols
else instance FlushColumnWordByHead "1" word cols
else instance FlushColumnWordByHead "2" word cols
else instance FlushColumnWordByHead "3" word cols
else instance FlushColumnWordByHead "4" word cols
else instance FlushColumnWordByHead "5" word cols
else instance FlushColumnWordByHead "6" word cols
else instance FlushColumnWordByHead "7" word cols
else instance FlushColumnWordByHead "8" word cols
else instance FlushColumnWordByHead "9" word cols
else instance Row.Cons word typ rest cols => FlushColumnWordByHead head word cols
else instance
  Fail (Beside (Beside (Text "Column ") (Quote word)) (Text " does not exist in the table")) =>
  FlushColumnWordByHead head word cols

class ValidateAfterName :: Symbol -> Symbol -> Row Type -> Constraint
class ValidateAfterName acc rest cols

instance FlushColumnWord acc cols => ValidateAfterName acc "" cols
else instance
  ( FlushColumnWord acc cols
  , Symbol.Cons h t rest
  , ValidateAfterNameByHead h t cols
  ) =>
  ValidateAfterName acc rest cols

class ValidateAfterNameByHead :: Symbol -> Symbol -> Row Type -> Constraint
class ValidateAfterNameByHead head tail cols

instance
  ( SkipSpaces tail rest
  , ValidateColumns rest cols
  ) =>
  ValidateAfterNameByHead "," tail cols

else instance
  ( Symbol.Append h t rest
  , ExtractWord rest word afterWord
  , HandleAfterColumnWord word afterWord cols
  ) =>
  ValidateAfterNameByHead h t cols

class HandleAfterColumnWord :: Symbol -> Symbol -> Row Type -> Constraint
class HandleAfterColumnWord word rest cols

instance SkipAlias rest cols => HandleAfterColumnWord "AS" rest cols
else instance SkipAlias rest cols => HandleAfterColumnWord "as" rest cols

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

instance FlushJoinWordByHead "$" word tables
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
else instance ResolveColumn word tables typ => FlushJoinWordByHead head word tables

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- ON CONFLICT
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

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Type-level IsAutoGenerated (Boolean kind)
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class IsAutoGeneratedTC :: Type -> Boolean -> Constraint
class IsAutoGeneratedTC constraints result | constraints -> result

instance IsAutoGeneratedTC (AutoIncrement a) True
else instance IsAutoGeneratedTC (Default s a) True
else instance IsAutoGeneratedTC a result => IsAutoGeneratedTC (PrimaryKey a) result
else instance IsAutoGeneratedTC a result => IsAutoGeneratedTC (Unique a) result
else instance IsAutoGeneratedTC a result => IsAutoGeneratedTC (ForeignKey t r c a) result
else instance IsAutoGeneratedTC a result => IsAutoGeneratedTC (Nullable a) result
else instance IsAutoGeneratedTC a False

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- InsertableColumnsRL: filter out auto-generated columns
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class InsertableColumnsRL :: RL.RowList Type -> RL.RowList Type -> Constraint
class InsertableColumnsRL tableRL outRL | tableRL -> outRL

instance InsertableColumnsRL RL.Nil RL.Nil
instance
  ( IsAutoGeneratedTC entry isAuto
  , ExtractType entry typ
  , InsertableColumnDecide isAuto name typ tail outRL
  ) =>
  InsertableColumnsRL (RL.Cons name entry tail) outRL

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
-- MakeNullableRL: wrap column types in Maybe for LEFT JOIN
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class MakeNullableRL :: RL.RowList Type -> RL.RowList Type -> Constraint
class MakeNullableRL rl out | rl -> out

instance MakeNullableRL RL.Nil RL.Nil
instance
  ( ExtractType entry typ
  , MakeNullableDecide typ name entry tail out
  ) =>
  MakeNullableRL (RL.Cons name entry tail) out

class MakeNullableDecide :: Type -> Symbol -> Type -> RL.RowList Type -> RL.RowList Type -> Constraint
class MakeNullableDecide typ name entry tail out | typ name entry tail -> out

-- Already nullable: keep as-is
instance MakeNullableRL tail out' => MakeNullableDecide (Maybe a) name entry tail (RL.Cons name entry out')
-- Not nullable: wrap in Nullable
else instance MakeNullableRL tail out' => MakeNullableDecide typ name entry tail (RL.Cons name (Nullable entry) out')

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Query builder: unified Q type
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

newtype Q :: Row (Row Type) -> Row Type -> Row Type -> Row Type -> Type
newtype Q tables result params stage = Q { sql :: String, values :: Array PG.PGValue }

class HasClause :: Symbol -> Row Type -> Constraint
class HasClause label row

instance Row.Cons label Unit rest row => HasClause label row

class HasAnyDML :: Row Type -> Constraint
class HasAnyDML stage

instance (RL.RowToList stage rl, HasAnyDMLRL rl) => HasAnyDML stage

class HasAnyDMLRL :: RL.RowList Type -> Constraint
class HasAnyDMLRL rl

instance HasAnyDMLRL (RL.Cons "select" Unit rest)
else instance HasAnyDMLRL (RL.Cons "set" Unit rest)
else instance HasAnyDMLRL (RL.Cons "delete" Unit rest)
else instance HasAnyDMLRL rest => HasAnyDMLRL (RL.Cons label typ rest)
else instance Fail (Text "WHERE requires a preceding SELECT, UPDATE (set), or DELETE") => HasAnyDMLRL RL.Nil

toSQL :: forall tables result params stage. Q tables result params stage -> String
toSQL (Q q) = q.sql

from :: forall name cols tables. IsSymbol name => Row.Cons name cols () tables => Proxy (Table name cols) -> Q tables () () ()
from _ = Q { sql: reflectSymbol (Proxy :: Proxy name), values: [] }

fromAs :: forall @alias name cols tables. IsSymbol name => IsSymbol alias => Row.Cons alias cols () tables => Proxy (Table name cols) -> Q tables () () ()
fromAs _ = Q { sql: reflectSymbol (Proxy :: Proxy name) <> " " <> reflectSymbol (Proxy :: Proxy alias), values: [] }

selectAll
  :: forall tables name cols result r p stage stage'
   . SingleTable tables name cols
  => IsSymbol name
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
  => Q tables r p stage
  -> Q tables result p stage'
selectAll (Q q) = Q (q { sql = "SELECT * FROM " <> q.sql })

select
  :: forall @sel tables result r p stage stage'
   . IsSymbol sel
  => ParseSelect sel tables result
  => Row.Lacks "select" stage
  => Row.Lacks "insert" stage
  => Row.Lacks "set" stage
  => Row.Lacks "delete" stage
  => Row.Lacks "where" stage
  => Row.Lacks "orderBy" stage
  => Row.Lacks "limit" stage
  => Row.Lacks "offset" stage
  => Row.Cons "select" Unit stage stage'
  => Q tables r p stage
  -> Q tables result p stage'
select (Q q) = Q (q { sql = "SELECT " <> reflectSymbol (Proxy :: Proxy sel) <> " FROM " <> q.sql })

selectDistinct
  :: forall @sel tables result r p stage stage'
   . IsSymbol sel
  => ParseSelect sel tables result
  => Row.Lacks "select" stage
  => Row.Lacks "insert" stage
  => Row.Lacks "set" stage
  => Row.Lacks "delete" stage
  => Row.Lacks "where" stage
  => Row.Lacks "orderBy" stage
  => Row.Lacks "limit" stage
  => Row.Lacks "offset" stage
  => Row.Cons "select" Unit stage stage'
  => Q tables r p stage
  -> Q tables result p stage'
selectDistinct (Q q) = Q (q { sql = "SELECT DISTINCT " <> reflectSymbol (Proxy :: Proxy sel) <> " FROM " <> q.sql })

selectDistinctOn
  :: forall @on @sel tables result r p stage stage'
   . IsSymbol on
  => IsSymbol sel
  => ValidateColumnList on tables
  => ParseSelect sel tables result
  => Row.Lacks "select" stage
  => Row.Lacks "insert" stage
  => Row.Lacks "set" stage
  => Row.Lacks "delete" stage
  => Row.Lacks "where" stage
  => Row.Lacks "orderBy" stage
  => Row.Lacks "limit" stage
  => Row.Lacks "offset" stage
  => Row.Cons "select" Unit stage stage'
  => Q tables r p stage
  -> Q tables result p stage'
selectDistinctOn (Q q) = Q (q { sql = "SELECT DISTINCT ON (" <> reflectSymbol (Proxy :: Proxy on) <> ") " <> reflectSymbol (Proxy :: Proxy sel) <> " FROM " <> q.sql })

where_
  :: forall @whr tables result params p stage stage'
   . IsSymbol whr
  => ParseWhere whr tables params
  => HasAnyDML stage
  => Row.Lacks "where" stage
  => Row.Lacks "insert" stage
  => Row.Lacks "groupBy" stage
  => Row.Lacks "having" stage
  => Row.Lacks "orderBy" stage
  => Row.Lacks "limit" stage
  => Row.Lacks "offset" stage
  => Row.Cons "where" Unit stage stage'
  => Q tables result p stage
  -> Q tables result params stage'
where_ (Q q) = Q (q { sql = q.sql <> " WHERE " <> reflectSymbol (Proxy :: Proxy whr) })

orderBy
  :: forall @cols tables result params stage stage'
   . IsSymbol cols
  => ValidateOrderBy cols tables
  => HasClause "select" stage
  => Row.Lacks "orderBy" stage
  => Row.Lacks "limit" stage
  => Row.Lacks "offset" stage
  => Row.Cons "orderBy" Unit stage stage'
  => Q tables result params stage
  -> Q tables result params stage'
orderBy (Q q) = Q (q { sql = q.sql <> " ORDER BY " <> reflectSymbol (Proxy :: Proxy cols) })

groupBy
  :: forall @cols tables result params stage stage'
   . IsSymbol cols
  => ValidateColumnList cols tables
  => HasClause "select" stage
  => Row.Lacks "groupBy" stage
  => Row.Lacks "having" stage
  => Row.Lacks "orderBy" stage
  => Row.Lacks "limit" stage
  => Row.Lacks "offset" stage
  => Row.Cons "groupBy" Unit stage stage'
  => Q tables result params stage
  -> Q tables result params stage'
groupBy (Q q) = Q (q { sql = q.sql <> " GROUP BY " <> reflectSymbol (Proxy :: Proxy cols) })

having
  :: forall @cond tables result params havingParams allParams stage stage'
   . IsSymbol cond
  => ParseWhere cond tables havingParams
  => Row.Union params havingParams allParams
  => Row.Nub allParams allParams
  => HasClause "groupBy" stage
  => Row.Lacks "having" stage
  => Row.Lacks "orderBy" stage
  => Row.Lacks "limit" stage
  => Row.Lacks "offset" stage
  => Row.Cons "having" Unit stage stage'
  => Q tables result params stage
  -> Q tables result allParams stage'
having (Q q) = Q (q { sql = q.sql <> " HAVING " <> reflectSymbol (Proxy :: Proxy cond) })

class IsLimitParam :: Symbol -> Symbol -> Boolean -> Constraint
class IsLimitParam head tail isParam | head tail -> isParam

instance IsLimitParam "$" tail True
else instance IsLimitParam head tail False

class ParseLimitOffsetParams :: Boolean -> Symbol -> Row Type -> Row Type -> Constraint
class ParseLimitOffsetParams isParam name params params' | isParam name params -> params'

instance
  ( Row.Lacks name params
  , Row.Cons name Int params params'
  ) =>
  ParseLimitOffsetParams True name params params'

instance ParseLimitOffsetParams False name params params

class ParseLimitOffset :: Symbol -> Row Type -> Row Type -> Constraint
class ParseLimitOffset sym params params' | sym params -> params'

instance
  ( Symbol.Cons head tail sym
  , IsLimitParam head tail isParam
  , ParseLimitOffsetParams isParam tail params params'
  ) =>
  ParseLimitOffset sym params params'

limit
  :: forall @sym tables result params params' stage stage'
   . ParseLimitOffset sym params params'
  => IsSymbol sym
  => HasClause "select" stage
  => Row.Lacks "limit" stage
  => Row.Cons "limit" Unit stage stage'
  => Q tables result params stage
  -> Q tables result params' stage'
limit (Q q) = Q (q { sql = q.sql <> " LIMIT " <> reflectSymbol (Proxy :: Proxy sym) })

offset
  :: forall @sym tables result params params' stage stage'
   . ParseLimitOffset sym params params'
  => IsSymbol sym
  => HasClause "select" stage
  => Row.Lacks "offset" stage
  => Row.Cons "offset" Unit stage stage'
  => Q tables result params stage
  -> Q tables result params' stage'
offset (Q q) = Q (q { sql = q.sql <> " OFFSET " <> reflectSymbol (Proxy :: Proxy sym) })

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- INSERT builder
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

insert
  :: forall tables name cols colsRL insertableRL insertable requiredRL required
       optionalProvided missing userRow userRowRL stage stage'
   . SingleTable tables name cols
  => RowToList cols colsRL
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
  -> Q tables () () stage
  -> Q tables () () stage'
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
  :: forall @sel tables name cols result p stage stage'
   . SingleTable tables name cols
  => IsSymbol sel
  => ParseSelect sel tables result
  => Row.Lacks "returning" stage
  => Row.Lacks "select" stage
  => Row.Cons "returning" Unit stage stage'
  => Q tables () p stage
  -> Q tables result p stage'
returning (Q q) = Q (q { sql = q.sql <> " RETURNING " <> reflectSymbol (Proxy :: Proxy sel) })

returningAll
  :: forall tables name cols result p stage stage'
   . SingleTable tables name cols
  => StripColumns cols result
  => Row.Lacks "returning" stage
  => Row.Lacks "select" stage
  => Row.Cons "returning" Unit stage stage'
  => Q tables () p stage
  -> Q tables result p stage'
returningAll (Q q) = Q (q { sql = q.sql <> " RETURNING *" })

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- UPDATE builder (SET)
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class ValidateSetColumnsRL :: RL.RowList Type -> Row Type -> Constraint
class ValidateSetColumnsRL rl cols

instance ValidateSetColumnsRL RL.Nil cols
instance
  ( Row.Cons name entry rest cols
  , ExtractType entry typ
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
  :: forall tables name cols setRow setRL stage stage'
   . SingleTable tables name cols
  => RowToList setRow setRL
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
  -> Q tables () () stage
  -> Q tables () () stage'
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
  :: forall tables name cols r p stage stage'
   . SingleTable tables name cols
  => IsSymbol name
  => Row.Lacks "select" stage
  => Row.Lacks "insert" stage
  => Row.Lacks "set" stage
  => Row.Lacks "delete" stage
  => Row.Cons "delete" Unit stage stage'
  => Q tables r p stage
  -> Q tables () () stage'
delete _ = Q { sql: "DELETE FROM " <> reflectSymbol (Proxy :: Proxy name), values: [] }

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- ON CONFLICT
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

onConflict
  :: forall @target @action tables name cols result params stage stage'
   . SingleTable tables name cols
  => IsSymbol target
  => IsSymbol action
  => ValidateColumns target cols
  => ParseConflictAction action cols
  => HasClause "insert" stage
  => Row.Lacks "conflict" stage
  => Row.Cons "conflict" Unit stage stage'
  => Q tables result params stage
  -> Q tables result params stage'
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
  :: forall @target tables name cols result params stage stage'
   . SingleTable tables name cols
  => IsSymbol target
  => ValidateColumns target cols
  => HasClause "insert" stage
  => Row.Lacks "conflict" stage
  => Row.Cons "conflict" Unit stage stage'
  => Q tables result params stage
  -> Q tables result params stage'
onConflictDoNothing (Q q) = Q
  ( q
      { sql = q.sql
          <> " ON CONFLICT ("
          <> reflectSymbol (Proxy :: Proxy target)
          <> ") DO NOTHING"
      }
  )

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- JOIN builders
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

innerJoin
  :: forall @cond name cols tables tables' r p stage
   . IsSymbol name
  => IsSymbol cond
  => Row.Lacks name tables
  => Row.Cons name cols tables tables'
  => ValidateJoinCondition cond tables'
  => Row.Lacks "select" stage
  => Row.Lacks "insert" stage
  => Row.Lacks "set" stage
  => Row.Lacks "delete" stage
  => Proxy (Table name cols)
  -> Q tables r p stage
  -> Q tables' () () (join :: Unit)
innerJoin _ (Q q) = Q
  { sql: q.sql
      <> " INNER JOIN "
      <> reflectSymbol (Proxy :: Proxy name)
      <> " ON "
      <> reflectSymbol (Proxy :: Proxy cond)
  , values: q.values
  }

leftJoin
  :: forall @cond name cols colsRL nullableColsRL nullableCols tables tables' r p stage
   . IsSymbol name
  => IsSymbol cond
  => RowToList cols colsRL
  => MakeNullableRL colsRL nullableColsRL
  => ListToRow nullableColsRL nullableCols
  => Row.Lacks name tables
  => Row.Cons name nullableCols tables tables'
  => ValidateJoinCondition cond tables'
  => Row.Lacks "select" stage
  => Row.Lacks "insert" stage
  => Row.Lacks "set" stage
  => Row.Lacks "delete" stage
  => Proxy (Table name cols)
  -> Q tables r p stage
  -> Q tables' () () (join :: Unit)
leftJoin _ (Q q) = Q
  { sql: q.sql
      <> " LEFT JOIN "
      <> reflectSymbol (Proxy :: Proxy name)
      <> " ON "
      <> reflectSymbol (Proxy :: Proxy cond)
  , values: q.values
  }

innerJoinAs
  :: forall @alias @cond name cols tables tables' r p stage
   . IsSymbol name
  => IsSymbol alias
  => IsSymbol cond
  => Row.Lacks alias tables
  => Row.Cons alias cols tables tables'
  => ValidateJoinCondition cond tables'
  => Row.Lacks "select" stage
  => Row.Lacks "insert" stage
  => Row.Lacks "set" stage
  => Row.Lacks "delete" stage
  => Proxy (Table name cols)
  -> Q tables r p stage
  -> Q tables' () () (join :: Unit)
innerJoinAs _ (Q q) = Q
  { sql: q.sql
      <> " INNER JOIN "
      <> reflectSymbol (Proxy :: Proxy name)
      <> " "
      <> reflectSymbol (Proxy :: Proxy alias)
      <> " ON "
      <> reflectSymbol (Proxy :: Proxy cond)
  , values: q.values
  }

leftJoinAs
  :: forall @alias @cond name cols colsRL nullableColsRL nullableCols tables tables' r p stage
   . IsSymbol name
  => IsSymbol alias
  => IsSymbol cond
  => RowToList cols colsRL
  => MakeNullableRL colsRL nullableColsRL
  => ListToRow nullableColsRL nullableCols
  => Row.Lacks alias tables
  => Row.Cons alias nullableCols tables tables'
  => ValidateJoinCondition cond tables'
  => Row.Lacks "select" stage
  => Row.Lacks "insert" stage
  => Row.Lacks "set" stage
  => Row.Lacks "delete" stage
  => Proxy (Table name cols)
  -> Q tables r p stage
  -> Q tables' () () (join :: Unit)
leftJoinAs _ (Q q) = Q
  { sql: q.sql
      <> " LEFT JOIN "
      <> reflectSymbol (Proxy :: Proxy name)
      <> " "
      <> reflectSymbol (Proxy :: Proxy alias)
      <> " ON "
      <> reflectSymbol (Proxy :: Proxy cond)
  , values: q.values
  }

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Set operations: UNION, INTERSECT, EXCEPT
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

type SetOpStage =
  ( select :: Unit
  , "where" :: Unit
  , groupBy :: Unit
  , having :: Unit
  , insert :: Unit
  , "set" :: Unit
  , "delete" :: Unit
  )

union
  :: forall tables1 tables2 result params1 params2 params stage1 stage2
   . HasClause "select" stage1
  => HasClause "select" stage2
  => Row.Union params1 params2 params
  => Row.Nub params params
  => Q tables1 result params1 stage1
  -> Q tables2 result params2 stage2
  -> Q tables1 result params SetOpStage
union (Q q1) (Q q2) = Q { sql: "(" <> q1.sql <> ") UNION (" <> q2.sql <> ")", values: q1.values <> q2.values }

unionAll
  :: forall tables1 tables2 result params1 params2 params stage1 stage2
   . HasClause "select" stage1
  => HasClause "select" stage2
  => Row.Union params1 params2 params
  => Row.Nub params params
  => Q tables1 result params1 stage1
  -> Q tables2 result params2 stage2
  -> Q tables1 result params SetOpStage
unionAll (Q q1) (Q q2) = Q { sql: "(" <> q1.sql <> ") UNION ALL (" <> q2.sql <> ")", values: q1.values <> q2.values }

intersect
  :: forall tables1 tables2 result params1 params2 params stage1 stage2
   . HasClause "select" stage1
  => HasClause "select" stage2
  => Row.Union params1 params2 params
  => Row.Nub params params
  => Q tables1 result params1 stage1
  -> Q tables2 result params2 stage2
  -> Q tables1 result params SetOpStage
intersect (Q q1) (Q q2) = Q { sql: "(" <> q1.sql <> ") INTERSECT (" <> q2.sql <> ")", values: q1.values <> q2.values }

intersectAll
  :: forall tables1 tables2 result params1 params2 params stage1 stage2
   . HasClause "select" stage1
  => HasClause "select" stage2
  => Row.Union params1 params2 params
  => Row.Nub params params
  => Q tables1 result params1 stage1
  -> Q tables2 result params2 stage2
  -> Q tables1 result params SetOpStage
intersectAll (Q q1) (Q q2) = Q { sql: "(" <> q1.sql <> ") INTERSECT ALL (" <> q2.sql <> ")", values: q1.values <> q2.values }

except_
  :: forall tables1 tables2 result params1 params2 params stage1 stage2
   . HasClause "select" stage1
  => HasClause "select" stage2
  => Row.Union params1 params2 params
  => Row.Nub params params
  => Q tables1 result params1 stage1
  -> Q tables2 result params2 stage2
  -> Q tables1 result params SetOpStage
except_ (Q q1) (Q q2) = Q { sql: "(" <> q1.sql <> ") EXCEPT (" <> q2.sql <> ")", values: q1.values <> q2.values }

exceptAll
  :: forall tables1 tables2 result params1 params2 params stage1 stage2
   . HasClause "select" stage1
  => HasClause "select" stage2
  => Row.Union params1 params2 params
  => Row.Nub params params
  => Q tables1 result params1 stage1
  -> Q tables2 result params2 stage2
  -> Q tables1 result params SetOpStage
exceptAll (Q q1) (Q q2) = Q { sql: "(" <> q1.sql <> ") EXCEPT ALL (" <> q2.sql <> ")", values: q1.values <> q2.values }

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
  :: forall tables result params paramsRL stage
   . RowToList params paramsRL
  => ParamsToArray paramsRL params
  => ReadForeign { | result }
  => PG.Connection
  -> { | params }
  -> Q tables result params stage
  -> Aff (Array { | result })
runQuery conn params (Q q) = do
  let entries = paramsToArray (Proxy :: Proxy paramsRL) params
  let { sql, values } = replaceNamedParams (Array.length q.values) entries q.sql
  let allValues = q.values <> values
  result <- PG.query (PG.SQL sql) allValues conn
  pure (unsafeDecodeRows result.rows)

runQueryOne
  :: forall tables result params paramsRL stage
   . RowToList params paramsRL
  => ParamsToArray paramsRL params
  => ReadForeign { | result }
  => PG.Connection
  -> { | params }
  -> Q tables result params stage
  -> Aff (Maybe { | result })
runQueryOne conn params (Q q) = do
  let entries = paramsToArray (Proxy :: Proxy paramsRL) params
  let { sql, values } = replaceNamedParams (Array.length q.values) entries q.sql
  let allValues = q.values <> values
  result <- PG.queryOne (PG.SQL sql) allValues conn
  pure (result <#> unsafeDecodeRow)

runExecute
  :: forall tables params paramsRL stage
   . RowToList params paramsRL
  => ParamsToArray paramsRL params
  => PG.Connection
  -> { | params }
  -> Q tables () params stage
  -> Aff Int
runExecute conn params (Q q) = do
  let entries = paramsToArray (Proxy :: Proxy paramsRL) params
  let { sql, values } = replaceNamedParams (Array.length q.values) entries q.sql
  let allValues = q.values <> values
  PG.execute (PG.SQL sql) allValues conn

-- Transaction variants

runQueryTx
  :: forall tables result params paramsRL stage
   . RowToList params paramsRL
  => ParamsToArray paramsRL params
  => ReadForeign { | result }
  => PG.Transaction
  -> { | params }
  -> Q tables result params stage
  -> Aff (Array { | result })
runQueryTx txn params (Q q) = do
  let entries = paramsToArray (Proxy :: Proxy paramsRL) params
  let { sql, values } = replaceNamedParams (Array.length q.values) entries q.sql
  let allValues = q.values <> values
  result <- PG.txQuery (PG.SQL sql) allValues txn
  pure (unsafeDecodeRows result.rows)

runExecuteTx
  :: forall tables params paramsRL stage
   . RowToList params paramsRL
  => ParamsToArray paramsRL params
  => PG.Transaction
  -> { | params }
  -> Q tables () params stage
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
