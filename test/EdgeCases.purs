module Test.Postgres.EdgeCases where

import Prelude

import Data.Maybe (Maybe)
import Prim.Boolean (True)
import Type.Function (type (#))
import Type.Proxy (Proxy(..))
import Yoga.Postgres.Schema

-- Same table definitions as Schema tests
type UsersTable = Table "users"
  ( id :: Int # PrimaryKey # AutoIncrement
  , name :: String
  , email :: String # Unique
  , age :: Maybe Int
  )

usersTable :: Proxy UsersTable
usersTable = Proxy

type PostsTable = Table "posts"
  ( id :: Int # PrimaryKey # AutoIncrement
  , title :: String
  , body :: String
  , user_id :: Int
  )

postsTable :: Proxy PostsTable
postsTable = Proxy

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- EDGE CASE 1: HAVING merges WHERE params (FIXED)
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

havingMergesWhereParams
  :: Q _ (name :: String, cnt :: Int) (age :: Int, minCount :: Int) _
havingMergesWhereParams = from usersTable
  # select @"name, COUNT(*) AS cnt"
  # where_ @"age > $age"
  # groupBy @"name"
  # having @"COUNT(*) > $minCount"

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- EDGE CASE 2: WHERE without any DML operation (FIXED)
-- from table # where_ now fails to compile (see WhereWithoutDML.purs)
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- EDGE CASE 3: GROUP BY doesn't enforce aggregation rules
-- Non-aggregated, non-grouped columns in SELECT should be
-- rejected when GROUP BY is present.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- This produces invalid SQL: SELECT name, email FROM users GROUP BY name
-- 'email' is not in GROUP BY and not aggregated.
-- PostgreSQL would reject this at runtime.
groupByMissingColumn
  :: Q _ (name :: String, email :: String) () _
groupByMissingColumn = from usersTable
  # select @"name, email"
  # groupBy @"name"

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- EDGE CASE 4: OVER clause content validated (FIXED)
-- Invalid OVER content now fails to compile
-- (see OverClauseInvalidContent.purs, OverClauseNonexistentColumn.purs)
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

overClauseValid
  :: Q _ (name :: String, rn :: Int) () _
overClauseValid = from usersTable
  # select @"name, ROW_NUMBER() OVER (ORDER BY age) AS rn"

overClausePartitionBy
  :: Q _ (name :: String, rnk :: Int) () _
overClausePartitionBy = from usersTable
  # select @"name, RANK() OVER (PARTITION BY name ORDER BY age DESC) AS rnk"

overClausePartitionByOnly
  :: Q _ (name :: String, rn :: Int) () _
overClausePartitionByOnly = from usersTable
  # select @"name, ROW_NUMBER() OVER (PARTITION BY name) AS rn"

overClauseEmpty
  :: Q _ (name :: String, rn :: Int) () _
overClauseEmpty = from usersTable
  # select @"name, ROW_NUMBER() OVER () AS rn"

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- EDGE CASE 5: WHERE keywords are case-sensitive (uppercase only)
-- "IS NULL" works but "is null" would fail (tries to resolve "is" as column)
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

uppercaseIsNull :: Q _ _ () _
uppercaseIsNull = from usersTable # selectAll # where_ @"age IS NULL"

lowercaseIsNull :: Q _ _ () _
lowercaseIsNull = from usersTable # selectAll # where_ @"age is null"

lowercaseNotIn :: Q _ _ (ids :: Array Int) _
lowercaseNotIn = from usersTable # selectAll # where_ @"id not in $ids"

lowercaseLike :: Q _ _ (pat :: String) _
lowercaseLike = from usersTable # selectAll # where_ @"name like $pat"

lowercaseBetween :: Q _ _ (lo :: Int, hi :: Int) _
lowercaseBetween = from usersTable # selectAll # where_ @"age between $lo and $hi"

lowercaseAndOr :: Q _ _ (minAge :: Int, namePat :: String) _
lowercaseAndOr = from usersTable # selectAll # where_ @"age > $minAge or name like $namePat"

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- EDGE CASE 6: DISTINCT ORDER BY on non-selected column
-- PostgreSQL: "ORDER BY items must appear in the select list"
-- when using SELECT DISTINCT
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- Produces: SELECT DISTINCT name FROM users ORDER BY age
-- PostgreSQL rejects this but it compiles
distinctOrderByNonSelected
  :: Q _ (name :: String) () _
distinctOrderByNonSelected = from usersTable
  # selectDistinct @"name"
  # orderBy @"age"

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- EDGE CASE 7: Nonexistent column in OVER (FIXED)
-- OVER content is now validated (see OverClauseNonexistentColumn.purs)
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- EDGE CASE 8: Nested parens in aggregate args (FIXED)
-- ExtractUntilParen now uses Peano depth counter for nesting
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

coalesceSimple
  :: Q _ (name :: String, val :: Int) () _
coalesceSimple = from usersTable
  # select @"name, COALESCE(age, 0) AS val"

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- EDGE CASE 9: INSERT on table with all auto-generated columns
-- Produces: INSERT INTO config () VALUES ()
-- This is invalid PostgreSQL syntax (should be DEFAULT VALUES)
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

type AllDefaultTable = Table "all_default"
  ( id :: Int # PrimaryKey # AutoIncrement
  , active :: Boolean # Default True
  , score :: Int # Default 0
  )

allDefaultInsert :: Q _ () () _
allDefaultInsert = from (Proxy :: Proxy AllDefaultTable)
  # insert {}
