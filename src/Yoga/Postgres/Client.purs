module Yoga.Postgres.Client where

import Prelude

import Data.Array (intercalate, mapWithIndex)
import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Effect.Aff (Aff)
import Prim.Boolean (True, False)
import Prim.Row (class Cons, class Lacks, class Union) as Row
import Prim.RowList as RL
import Prim.RowList (class RowToList)
import Prim.TypeError (class Fail, Text)
import Type.Proxy (Proxy(..))
import Type.RowList (class ListToRow)
import Yoga.JSON (class ReadForeign)
import Yoga.Postgres as PG
import Yoga.Postgres.Schema
  ( class ColumnCountRL, class ColumnNamesRL, class ExtractType
  , class FieldToPGValue, class InsertableColumnsRL, class RecordValuesRL
  , class RequiredColumnsRL, class SetClauseRL, class StripColumnsRL
  , class ValidateSetColumnsRL
  , Table, PrimaryKey, AutoIncrement, Unique, Default
  , ForeignKey, References, Nullable
  , columnCountRL, columnNamesRL, fieldToPGValue, recordValuesRL, setClauseRL
  , unsafeDecodeRow, unsafeDecodeRows
  )

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Type extraction from tables
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class TableRow :: Type -> Row Type -> Constraint
class TableRow table row | table -> row

instance
  ( RowToList cols rl
  , StripColumnsRL rl outRL
  , ListToRow outRL row
  ) =>
  TableRow (Table name cols) row

class InsertableRow :: Type -> Row Type -> Constraint
class InsertableRow table row | table -> row

instance
  ( RowToList cols rl
  , InsertableColumnsRL rl insertRL
  , ListToRow insertRL row
  ) =>
  InsertableRow (Table name cols) row

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Primary key extraction
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class HasPrimaryKeyConstraint :: Type -> Boolean -> Constraint
class HasPrimaryKeyConstraint a result | a -> result

instance HasPrimaryKeyConstraint (PrimaryKey a) True
else instance HasPrimaryKeyConstraint a result => HasPrimaryKeyConstraint (AutoIncrement a) result
else instance HasPrimaryKeyConstraint a result => HasPrimaryKeyConstraint (Unique a) result
else instance HasPrimaryKeyConstraint a result => HasPrimaryKeyConstraint (Default s a) result
else instance HasPrimaryKeyConstraint a result => HasPrimaryKeyConstraint (ForeignKey t r c a) result
else instance HasPrimaryKeyConstraint a result => HasPrimaryKeyConstraint (Nullable a) result
else instance HasPrimaryKeyConstraint a False

class FindPrimaryKeyRL :: RL.RowList Type -> Symbol -> Type -> Constraint
class FindPrimaryKeyRL rl colName colType | rl -> colName colType

instance Fail (Text "Table has no primary key column") => FindPrimaryKeyRL RL.Nil "" Unit

instance
  ( HasPrimaryKeyConstraint entry isPK
  , FindPrimaryKeyDecide isPK name entry tail colName colType
  ) =>
  FindPrimaryKeyRL (RL.Cons name entry tail) colName colType

class FindPrimaryKeyDecide :: Boolean -> Symbol -> Type -> RL.RowList Type -> Symbol -> Type -> Constraint
class FindPrimaryKeyDecide isPK name entry tail colName colType | isPK name entry tail -> colName colType

instance ExtractType entry colType => FindPrimaryKeyDecide True name entry tail name colType
instance FindPrimaryKeyRL tail colName colType => FindPrimaryKeyDecide False name entry tail colName colType

class FindPrimaryKey :: Type -> Symbol -> Type -> Constraint
class FindPrimaryKey table colName colType | table -> colName colType

instance
  ( RowToList cols rl
  , FindPrimaryKeyRL rl colName colType
  ) =>
  FindPrimaryKey (Table name cols) colName colType

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Foreign key detection for auto-joins
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

class HasForeignKeyTo :: Type -> Symbol -> Boolean -> Constraint
class HasForeignKeyTo colType tableName result | colType tableName -> result

instance HasForeignKeyTo (ForeignKey table References col a) table True
else instance HasForeignKeyTo a tn result => HasForeignKeyTo (PrimaryKey a) tn result
else instance HasForeignKeyTo a tn result => HasForeignKeyTo (AutoIncrement a) tn result
else instance HasForeignKeyTo a tn result => HasForeignKeyTo (Unique a) tn result
else instance HasForeignKeyTo a tn result => HasForeignKeyTo (Default s a) tn result
else instance HasForeignKeyTo a tn result => HasForeignKeyTo (Nullable a) tn result
else instance HasForeignKeyTo a tn False

class ExtractForeignKeyCol :: Type -> Symbol -> Symbol -> Constraint
class ExtractForeignKeyCol colType tableName refCol | colType tableName -> refCol

instance ExtractForeignKeyCol (ForeignKey table References col a) table col
else instance ExtractForeignKeyCol a tn col => ExtractForeignKeyCol (PrimaryKey a) tn col
else instance ExtractForeignKeyCol a tn col => ExtractForeignKeyCol (AutoIncrement a) tn col
else instance ExtractForeignKeyCol a tn col => ExtractForeignKeyCol (Unique a) tn col
else instance ExtractForeignKeyCol a tn col => ExtractForeignKeyCol (Default s a) tn col
else instance ExtractForeignKeyCol a tn col => ExtractForeignKeyCol (Nullable a) tn col

class FindForeignKeyToRL :: RL.RowList Type -> Symbol -> Symbol -> Symbol -> Constraint
class FindForeignKeyToRL childRL parentName localCol parentCol | childRL parentName -> localCol parentCol

instance
  Fail (Text "No foreign key found referencing the target table")
  => FindForeignKeyToRL RL.Nil parentName "" ""

instance
  ( HasForeignKeyTo entry parentName hasFK
  , FindForeignKeyToDecide hasFK name entry tail parentName localCol parentCol
  ) =>
  FindForeignKeyToRL (RL.Cons name entry tail) parentName localCol parentCol

class FindForeignKeyToDecide :: Boolean -> Symbol -> Type -> RL.RowList Type -> Symbol -> Symbol -> Symbol -> Constraint
class FindForeignKeyToDecide hasFK name entry tail parentName localCol parentCol
  | hasFK name entry tail parentName -> localCol parentCol

instance
  ExtractForeignKeyCol entry parentName parentCol
  => FindForeignKeyToDecide True name entry tail parentName name parentCol

instance
  FindForeignKeyToRL tail parentName localCol parentCol
  => FindForeignKeyToDecide False name entry tail parentName localCol parentCol

class FindForeignKeyTo :: Type -> Symbol -> Symbol -> Symbol -> Constraint
class FindForeignKeyTo childTable parentTableName localCol parentCol
  | childTable parentTableName -> localCol parentCol

instance
  ( RowToList cols rl
  , FindForeignKeyToRL rl parentName localCol parentCol
  ) =>
  FindForeignKeyTo (Table name cols) parentName localCol parentCol

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- CRUD operations
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

findAll
  :: forall name cols row
   . IsSymbol name
  => TableRow (Table name cols) row
  => ReadForeign { | row }
  => Proxy (Table name cols)
  -> PG.Connection
  -> Aff (Array { | row })
findAll _ conn = do
  result <- PG.query (PG.SQL sql) [] conn
  pure (unsafeDecodeRows result.rows)
  where
  sql = "SELECT * FROM " <> reflectSymbol (Proxy :: Proxy name)

findById
  :: forall name cols row pkCol pkType
   . IsSymbol name
  => IsSymbol pkCol
  => TableRow (Table name cols) row
  => FindPrimaryKey (Table name cols) pkCol pkType
  => FieldToPGValue pkType
  => ReadForeign { | row }
  => Proxy (Table name cols)
  -> PG.Connection
  -> pkType
  -> Aff (Maybe { | row })
findById _ conn pk = do
  result <- PG.queryOne (PG.SQL sql) [ fieldToPGValue pk ] conn
  pure (result <#> unsafeDecodeRow)
  where
  sql = "SELECT * FROM " <> reflectSymbol (Proxy :: Proxy name)
    <> " WHERE " <> reflectSymbol (Proxy :: Proxy pkCol) <> " = $1"

create
  :: forall name cols colsRL insertableRL insertable requiredRL required
       optionalProvided missing userRow userRowRL row
   . IsSymbol name
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
  => TableRow (Table name cols) row
  => ReadForeign { | row }
  => Proxy (Table name cols)
  -> PG.Connection
  -> { | userRow }
  -> Aff (Maybe { | row })
create _ conn rec = do
  result <- PG.queryOne (PG.SQL sql) values conn
  pure (result <#> unsafeDecodeRow)
  where
  colNames = columnNamesRL (Proxy :: Proxy userRowRL)
  placeholders = colNames # mapWithIndex \i _ -> "$" <> show (i + 1)
  sql = "INSERT INTO " <> reflectSymbol (Proxy :: Proxy name)
    <> " (" <> intercalate ", " colNames <> ")"
    <> " VALUES (" <> intercalate ", " placeholders <> ")"
    <> " RETURNING *"
  values = recordValuesRL (Proxy :: Proxy userRowRL) rec

updateById
  :: forall name cols row pkCol pkType setRow setRL
   . IsSymbol name
  => IsSymbol pkCol
  => TableRow (Table name cols) row
  => FindPrimaryKey (Table name cols) pkCol pkType
  => RowToList setRow setRL
  => ValidateSetColumnsRL setRL cols
  => SetClauseRL setRL
  => ColumnCountRL setRL
  => RecordValuesRL setRL setRow
  => FieldToPGValue pkType
  => ReadForeign { | row }
  => Proxy (Table name cols)
  -> PG.Connection
  -> pkType
  -> { | setRow }
  -> Aff (Maybe { | row })
updateById _ conn pk rec = do
  result <- PG.queryOne (PG.SQL sql) values conn
  pure (result <#> unsafeDecodeRow)
  where
  setClauses = setClauseRL (Proxy :: Proxy setRL) 1
  setCount = columnCountRL (Proxy :: Proxy setRL)
  sql = "UPDATE " <> reflectSymbol (Proxy :: Proxy name)
    <> " SET " <> intercalate ", " setClauses
    <> " WHERE " <> reflectSymbol (Proxy :: Proxy pkCol) <> " = $" <> show (setCount + 1)
    <> " RETURNING *"
  values = recordValuesRL (Proxy :: Proxy setRL) rec <> [ fieldToPGValue pk ]

deleteById
  :: forall name cols pkCol pkType
   . IsSymbol name
  => IsSymbol pkCol
  => FindPrimaryKey (Table name cols) pkCol pkType
  => FieldToPGValue pkType
  => Proxy (Table name cols)
  -> PG.Connection
  -> pkType
  -> Aff Int
deleteById _ conn pk =
  PG.execute (PG.SQL sql) [ fieldToPGValue pk ] conn
  where
  sql = "DELETE FROM " <> reflectSymbol (Proxy :: Proxy name)
    <> " WHERE " <> reflectSymbol (Proxy :: Proxy pkCol) <> " = $1"

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Belongs-to join (many-to-one via row_to_json)
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

findWith
  :: forall childName childCols parentName parentCols
       childRow parentRow localCol parentCol rest result
   . IsSymbol childName
  => IsSymbol parentName
  => IsSymbol localCol
  => IsSymbol parentCol
  => TableRow (Table childName childCols) childRow
  => TableRow (Table parentName parentCols) parentRow
  => FindForeignKeyTo (Table childName childCols) parentName localCol parentCol
  => Row.Cons parentName { | parentRow } () rest
  => Row.Cons childName { | childRow } rest result
  => ReadForeign { | result }
  => Proxy (Table childName childCols)
  -> Proxy (Table parentName parentCols)
  -> PG.Connection
  -> Aff (Array { | result })
findWith _ _ conn = do
  result <- PG.query (PG.SQL sql) [] conn
  pure (unsafeDecodeRows result.rows)
  where
  childTbl = reflectSymbol (Proxy :: Proxy childName)
  parentTbl = reflectSymbol (Proxy :: Proxy parentName)
  localC = reflectSymbol (Proxy :: Proxy localCol)
  parentC = reflectSymbol (Proxy :: Proxy parentCol)
  sql = "SELECT row_to_json(c.*) AS \"" <> childTbl
    <> "\", row_to_json(p.*) AS \"" <> parentTbl <> "\""
    <> " FROM " <> childTbl <> " c"
    <> " INNER JOIN " <> parentTbl <> " p ON c." <> localC <> " = p." <> parentC

findByIdWith
  :: forall childName childCols parentName parentCols
       childRow parentRow localCol parentCol pkCol pkType rest result
   . IsSymbol childName
  => IsSymbol parentName
  => IsSymbol localCol
  => IsSymbol parentCol
  => IsSymbol pkCol
  => TableRow (Table childName childCols) childRow
  => TableRow (Table parentName parentCols) parentRow
  => FindForeignKeyTo (Table childName childCols) parentName localCol parentCol
  => FindPrimaryKey (Table childName childCols) pkCol pkType
  => FieldToPGValue pkType
  => Row.Cons parentName { | parentRow } () rest
  => Row.Cons childName { | childRow } rest result
  => ReadForeign { | result }
  => Proxy (Table childName childCols)
  -> Proxy (Table parentName parentCols)
  -> PG.Connection
  -> pkType
  -> Aff (Maybe { | result })
findByIdWith _ _ conn pk = do
  result <- PG.queryOne (PG.SQL sql) [ fieldToPGValue pk ] conn
  pure (result <#> unsafeDecodeRow)
  where
  childTbl = reflectSymbol (Proxy :: Proxy childName)
  parentTbl = reflectSymbol (Proxy :: Proxy parentName)
  localC = reflectSymbol (Proxy :: Proxy localCol)
  parentC = reflectSymbol (Proxy :: Proxy parentCol)
  pkColName = reflectSymbol (Proxy :: Proxy pkCol)
  sql = "SELECT row_to_json(c.*) AS \"" <> childTbl
    <> "\", row_to_json(p.*) AS \"" <> parentTbl <> "\""
    <> " FROM " <> childTbl <> " c"
    <> " INNER JOIN " <> parentTbl <> " p ON c." <> localC <> " = p." <> parentC
    <> " WHERE c." <> pkColName <> " = $1"

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Has-many include (one-to-many via json_agg subquery)
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

findIncluding
  :: forall @label parentName parentCols childName childCols
       parentRow childRow localCol parentCol result
   . IsSymbol label
  => IsSymbol parentName
  => IsSymbol childName
  => IsSymbol localCol
  => IsSymbol parentCol
  => TableRow (Table parentName parentCols) parentRow
  => TableRow (Table childName childCols) childRow
  => FindForeignKeyTo (Table childName childCols) parentName localCol parentCol
  => Row.Cons label (Array { | childRow }) parentRow result
  => Row.Lacks label parentRow
  => ReadForeign { | result }
  => Proxy (Table parentName parentCols)
  -> Proxy (Table childName childCols)
  -> PG.Connection
  -> Aff (Array { | result })
findIncluding _ _ conn = do
  result <- PG.query (PG.SQL sql) [] conn
  pure (unsafeDecodeRows result.rows)
  where
  parentTbl = reflectSymbol (Proxy :: Proxy parentName)
  childTbl = reflectSymbol (Proxy :: Proxy childName)
  localC = reflectSymbol (Proxy :: Proxy localCol)
  parentC = reflectSymbol (Proxy :: Proxy parentCol)
  lbl = reflectSymbol (Proxy :: Proxy label)
  sql = "SELECT p.*, COALESCE((SELECT json_agg(to_jsonb(c.*)) FROM "
    <> childTbl <> " c WHERE c." <> localC <> " = p." <> parentC
    <> "), '[]'::json) AS \"" <> lbl <> "\""
    <> " FROM " <> parentTbl <> " p"

findByIdIncluding
  :: forall @label parentName parentCols childName childCols
       parentRow childRow localCol parentCol pkCol pkType result
   . IsSymbol label
  => IsSymbol parentName
  => IsSymbol childName
  => IsSymbol localCol
  => IsSymbol parentCol
  => IsSymbol pkCol
  => TableRow (Table parentName parentCols) parentRow
  => TableRow (Table childName childCols) childRow
  => FindForeignKeyTo (Table childName childCols) parentName localCol parentCol
  => FindPrimaryKey (Table parentName parentCols) pkCol pkType
  => FieldToPGValue pkType
  => Row.Cons label (Array { | childRow }) parentRow result
  => Row.Lacks label parentRow
  => ReadForeign { | result }
  => Proxy (Table parentName parentCols)
  -> Proxy (Table childName childCols)
  -> PG.Connection
  -> pkType
  -> Aff (Maybe { | result })
findByIdIncluding _ _ conn pk = do
  result <- PG.queryOne (PG.SQL sql) [ fieldToPGValue pk ] conn
  pure (result <#> unsafeDecodeRow)
  where
  parentTbl = reflectSymbol (Proxy :: Proxy parentName)
  childTbl = reflectSymbol (Proxy :: Proxy childName)
  localC = reflectSymbol (Proxy :: Proxy localCol)
  parentC = reflectSymbol (Proxy :: Proxy parentCol)
  pkColName = reflectSymbol (Proxy :: Proxy pkCol)
  lbl = reflectSymbol (Proxy :: Proxy label)
  sql = "SELECT p.*, COALESCE((SELECT json_agg(to_jsonb(c.*)) FROM "
    <> childTbl <> " c WHERE c." <> localC <> " = p." <> parentC
    <> "), '[]'::json) AS \"" <> lbl <> "\""
    <> " FROM " <> parentTbl <> " p"
    <> " WHERE p." <> pkColName <> " = $1"
