module Yoga.Postgres.TypedQuery where

import Prelude

import Data.Either (Either(..))
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Foreign (Foreign)
import Heterogeneous.Folding (class HFoldlWithIndex)
import Yoga.Postgres as PG
import Yoga.SQL.PostgresTypes (class ToSQLParam, SQLParameter, SQLQuery, TurnIntoSQLParam, argsFor, sqlQueryToString)
import Unsafe.Coerce (unsafeCoerce)
import Yoga.JSON (class ReadForeign)
import Yoga.JSON as JSON

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Convert SQL Types to Postgres Types
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | Convert a SQLParameter to a PG.PGValue
sqlParamToPGValue :: SQLParameter -> PG.PGValue
sqlParamToPGValue = unsafeCoerce

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Type-Safe Query Execution
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | Execute a typed SQL query and parse results with yoga-json
-- | 
-- | Example:
-- |   query = sql $ "SELECT * FROM users WHERE age > " ^ int @"minAge" ^ " AND name = " ^ str @"name"
-- |   users <- executeSql @User query { minAge: 25, name: "Alice" } conn
executeSql
  :: forall @params @result
   . HFoldlWithIndex TurnIntoSQLParam (Map String SQLParameter) { | params } (Map String SQLParameter)
  => ReadForeign result
  => SQLQuery params
  -> { | params }
  -> PG.Connection
  -> Aff (Either String (Array result))
executeSql sqlQuery params conn = do
  let
    sql = PG.SQL (sqlQueryToString sqlQuery)
    sqlParams = argsFor sqlQuery params
    pgParams = map sqlParamToPGValue sqlParams
  result <- PG.query sql pgParams conn
  pure $ traverse parseRow result.rows
  where
  parseRow :: Foreign -> Either String result
  parseRow row = case (JSON.read row :: Either _ result) of
    Left errors -> Left (show errors)
    Right parsed -> Right parsed

-- | Execute a typed SQL query with no result parsing (raw Foreign)
executeSqlRaw
  :: forall @params
   . HFoldlWithIndex TurnIntoSQLParam (Map String SQLParameter) { | params } (Map String SQLParameter)
  => SQLQuery params
  -> { | params }
  -> PG.Connection
  -> Aff PG.QueryResult
executeSqlRaw sqlQuery params conn = do
  let
    sql = PG.SQL (sqlQueryToString sqlQuery)
    sqlParams = argsFor sqlQuery params
    pgParams = map sqlParamToPGValue sqlParams
  PG.query sql pgParams conn

-- | Execute a typed SQL query and return a single row
executeSqlOne
  :: forall @params @result
   . HFoldlWithIndex TurnIntoSQLParam (Map String SQLParameter) { | params } (Map String SQLParameter)
  => ReadForeign result
  => SQLQuery params
  -> { | params }
  -> PG.Connection
  -> Aff (Either String (Maybe result))
executeSqlOne sqlQuery params conn = do
  result <- executeSql @params @result sqlQuery params conn
  pure $ result <#> case _ of
    [ x ] -> Just x
    [] -> Nothing
    _ -> Nothing -- More than one result, return Nothing

-- | Execute a mutation (INSERT/UPDATE/DELETE)
executeMutation
  :: forall @params
   . HFoldlWithIndex TurnIntoSQLParam (Map String SQLParameter) { | params } (Map String SQLParameter)
  => SQLQuery params
  -> { | params }
  -> PG.Connection
  -> Aff Int
executeMutation sqlQuery params conn = do
  let
    sql = PG.SQL (sqlQueryToString sqlQuery)
    sqlParams = argsFor sqlQuery params
    pgParams = map sqlParamToPGValue sqlParams
  PG.execute sql pgParams conn
