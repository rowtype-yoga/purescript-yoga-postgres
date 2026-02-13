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
-- EDGE CASE 2: WHERE without any DML operation
-- from table # where_ compiles, producing invalid SQL like
-- "users WHERE id = $id" (missing SELECT/DELETE/UPDATE)
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

whereWithoutOperation :: Q _ _ (id :: Int) _
whereWithoutOperation = from usersTable # where_ @"id = $id"

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
-- EDGE CASE 4: OVER clause content is not validated
-- Invalid SQL inside OVER() parens is silently accepted.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- OVER clause content is completely skipped by ExtractUntilParen
-- "INVALID SQL HERE" is not validated at all
overClauseNotValidated
  :: Q _ (name :: String, rn :: Int) () _
overClauseNotValidated = from usersTable
  # select @"name, ROW_NUMBER() OVER (THIS IS TOTALLY INVALID) AS rn"

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
-- EDGE CASE 7: Aggregate function on nonexistent column in OVER
-- OVER content is skipped so "ORDER BY nonexistent" compiles
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

overNonexistentColumn
  :: Q _ (name :: String, rn :: Int) () _
overNonexistentColumn = from usersTable
  # select @"name, ROW_NUMBER() OVER (ORDER BY nonexistent_column) AS rn"

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- EDGE CASE 8: Nested parens in aggregate args
-- ExtractUntilParen stops at the FIRST ), not matching brackets
-- COALESCE(NULLIF(x, 0), 1) would break parsing
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- Uncomment to test:
-- nestedParensInAggregate
--   :: Q _ (name :: String, val :: Int) () _
-- nestedParensInAggregate = from usersTable
--   # select @"name, COALESCE(age, 0) AS val"

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
