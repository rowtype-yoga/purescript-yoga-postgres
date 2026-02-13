module Yoga.Postgres.Schema where

import Prelude

import Data.Array (intercalate, mapWithIndex, filter)
import Data.String as String
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

-- Type-level column name validation
-- Validates that every comma-separated name in a Symbol exists in the table's row

class ValidateColumns :: Symbol -> Row Type -> Constraint
class ValidateColumns sym cols

class ValidateColumnsGo :: Symbol -> Symbol -> Symbol -> Row Type -> Constraint
class ValidateColumnsGo head tail acc cols

-- Empty string: nothing to validate
instance ValidateColumns "" cols

-- Non-empty: start character-by-character walk
else instance
  ( Symbol.Cons head tail sym
  , ValidateColumnsGo head tail "" cols
  ) =>
  ValidateColumns sym cols

-- Hit a comma: validate accumulated name, skip to next
instance
  ( ValidateColumn acc cols
  , SkipSpaces tail rest
  , ValidateColumns rest cols
  ) =>
  ValidateColumnsGo "," tail acc cols

-- Hit a space: validate accumulated name, skip spaces then expect comma or end
else instance
  ( SkipSpaces tail rest
  , ValidateAfterSpace acc rest cols
  ) =>
  ValidateColumnsGo " " tail acc cols

-- End of string (no more chars): validate final accumulated name
else instance
  ( Symbol.Append acc h acc'
  , ValidateColumn acc' cols
  ) =>
  ValidateColumnsGo h "" acc cols

-- Regular character: append to accumulator, continue
else instance
  ( Symbol.Append acc h acc'
  , Symbol.Cons nextH nextT tail
  , ValidateColumnsGo nextH nextT acc' cols
  ) =>
  ValidateColumnsGo h tail acc cols

-- After a space inside a name: either comma (end of name) or more text
class ValidateAfterSpace :: Symbol -> Symbol -> Row Type -> Constraint
class ValidateAfterSpace acc rest cols

-- Rest is empty: validate the accumulated name
instance ValidateColumn acc cols => ValidateAfterSpace acc "" cols

-- Rest starts with comma: validate name, continue parsing
else instance
  ( ValidateColumn acc cols
  , Symbol.Cons "," tail rest
  , SkipSpaces tail rest'
  , ValidateColumns rest' cols
  ) =>
  ValidateAfterSpace acc rest cols

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

-- Validate a single column name exists in the row
class ValidateColumn :: Symbol -> Row Type -> Constraint
class ValidateColumn name cols

instance
  ( Row.Cons name typ rest cols
  ) =>
  ValidateColumn name cols

else instance
  ( Fail (Beside (Beside (Text "Column ") (Quote name)) (Text " does not exist in the table"))
  ) =>
  ValidateColumn name cols

-- Reflect a comma-separated Symbol to Array String at value level
splitColumns :: forall sym. IsSymbol sym => Proxy sym -> Array String
splitColumns _ = reflectSymbol (Proxy :: Proxy sym)
  # String.split (String.Pattern ",")
  # map String.trim
  # filter (_ /= "")

-- Query builder

newtype Q :: Symbol -> Row Type -> Type
newtype Q name cols = Q String

from :: forall name cols. Proxy (Table name cols) -> Q name cols
from _ = Q ""

selectAll :: forall name cols. IsSymbol name => Q name cols -> Q name cols
selectAll _ = Q ("SELECT * FROM " <> reflectSymbol (Proxy :: Proxy name))

select
  :: forall @sel name cols
   . IsSymbol name
  => IsSymbol sel
  => ValidateColumns sel cols
  => Q name cols
  -> Q name cols
select _ = Q ("SELECT " <> reflectSymbol (Proxy :: Proxy sel) <> " FROM " <> reflectSymbol (Proxy :: Proxy name))

where_
  :: forall @whr name cols
   . IsSymbol whr
  => ValidateColumns whr cols
  => Q name cols
  -> Q name cols
where_ (Q base) = do
  let cols = splitColumns (Proxy :: Proxy whr)
  let conditions = cols # mapWithIndex \i col -> col <> " = $" <> show (i + 1)
  Q (base <> " WHERE " <> intercalate " AND " conditions)

toSQL :: forall name cols. Q name cols -> String
toSQL (Q s) = s
