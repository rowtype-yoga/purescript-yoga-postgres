module Test.Postgres.Schema where

import Prelude

import Data.Date (canonicalDate)
import Data.DateTime (DateTime(..))
import Data.Enum (toEnum)
import Data.Maybe (Maybe(..), fromJust)
import Data.Time (Time(..))
import Partial.Unsafe (unsafePartial)

import Type.Function (type (#))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (Foreign, unsafeToForeign)
import JS.BigInt (BigInt)
import JS.BigInt as BigInt
import Prim.Boolean (True)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))
import Yoga.Postgres as PG
import Yoga.Postgres.Schema

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Phase 1: Table type definitions compile
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

type UsersTable = Table "users"
  ( id :: Int # PrimaryKey # AutoIncrement
  , name :: String
  , email :: String # Unique
  , age :: Maybe Int
  )

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Phase 2: CREATE TABLE DDL generation
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

ddl :: String
ddl = createTableDDL @UsersTable

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Phase 3: Type-safe INSERT
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

insertSQL :: String
insertSQL = insertSQLFor @UsersTable

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Phase 4: Type-safe SELECT
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

selectAllSQL :: String
selectAllSQL = selectAllSQLFor @UsersTable

selectWhereSQL :: String
selectWhereSQL = selectWhereSQLFor @UsersTable @(id :: Int)

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Phase 5: Type-level defaults
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

type ConfigTable = Table "config"
  ( id :: Int # PrimaryKey # AutoIncrement
  , active :: Boolean # Default True
  , role :: String # Default "user"
  , score :: Int # Default 0
  )

configDDL :: String
configDDL = createTableDDL @ConfigTable

configInsertSQL :: String
configInsertSQL = insertSQLFor @ConfigTable

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Extended types: DateTime, BigInt, Jsonb, Array
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

type EventsTable = Table "events"
  ( id :: Int # PrimaryKey # AutoIncrement
  , title :: String
  , metadata :: Jsonb
  , tags :: Array String
  , created_at :: DateTime
  , view_count :: BigInt
  )

eventsTable :: Proxy EventsTable
eventsTable = Proxy

eventsDDL :: String
eventsDDL = createTableDDL @EventsTable

typedJsonbWhere
  :: Q _ _ (metadata :: Jsonb) _
typedJsonbWhere = from eventsTable # selectAll # where_ @"metadata @> $metadata"

typedJsonbInvalid
  :: Q _ _ (metadata :: Jsonb) _
typedJsonbInvalid = from eventsTable # selectAll # where_ @"metadata !* $metadata"

typedTitleLike
  :: Q _ _ (title :: String) _
typedTitleLike = from eventsTable # selectAll # where_ @"title LIKE $title"

typedArrayWhere
  :: Q _ _ (id :: Array Int) _
typedArrayWhere = from eventsTable # selectAll # where_ @"id = ANY($id)"

testDateTime :: DateTime
testDateTime = unsafePartial do
  let year = fromJust (toEnum 2025)
  let month = fromJust (toEnum 1)
  let day = fromJust (toEnum 15)
  let hour = fromJust (toEnum 12)
  let minute = fromJust (toEnum 0)
  let second = fromJust (toEnum 0)
  let ms = fromJust (toEnum 0)
  DateTime (canonicalDate year month day) (Time hour minute second ms)

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Phase 6: Type-safe UPDATE
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

updateSQL :: String
updateSQL = updateSQLFor @UsersTable @(name :: String) @(id :: Int)

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Phase 7: Type-safe DELETE
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

deleteSQL :: String
deleteSQL = deleteSQLFor @UsersTable @(id :: Int)

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Phase 8: Builder-style query API
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

usersTable :: Proxy UsersTable
usersTable = Proxy

typedSelectAll
  :: Q _
       (age :: Maybe Int, email :: String, id :: Int, name :: String)
       ()
       _
typedSelectAll = from usersTable # selectAll

typedSelectCols
  :: Q _
       (name :: String, email :: String)
       ()
       _
typedSelectCols = from usersTable # select @"name, email"

typedSelectAlias
  :: Q _
       (name :: String, e :: String)
       ()
       _
typedSelectAlias = from usersTable # select @"name, email AS e"

typedWhere
  :: Q _
       (age :: Maybe Int, email :: String, id :: Int, name :: String)
       (id :: Int)
       _
typedWhere = from usersTable # selectAll # where_ @"id = $id"

typedWhereComplex
  :: Q _
       (age :: Maybe Int, email :: String, id :: Int, name :: String)
       (name :: String, age :: Int)
       _
typedWhereComplex = from usersTable # selectAll # where_ @"name = $name AND age > $age"

typedWhereStringLiteral
  :: Q _
       (age :: Maybe Int, email :: String, id :: Int, name :: String)
       (status :: String)
       _
typedWhereStringLiteral = from usersTable # selectAll # where_ @"name = $status AND email != 'test@example.com'"

typedSelectColsWhere
  :: Q _
       (name :: String)
       (age :: Int)
       _
typedSelectColsWhere = from usersTable # select @"name" # where_ @"age > $age"

builderSelectAll :: String
builderSelectAll = typedSelectAll # toSQL

builderSelectCols :: String
builderSelectCols = typedSelectCols # toSQL

builderSelectAlias :: String
builderSelectAlias = typedSelectAlias # toSQL

builderSelectWhere :: String
builderSelectWhere = typedWhere # toSQL

builderSelectColsWhere :: String
builderSelectColsWhere = typedSelectColsWhere # toSQL

builderWhereComplex :: String
builderWhereComplex = typedWhereComplex # toSQL

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Phase 9: Builder INSERT / UPDATE / ON CONFLICT
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

typedInsert
  :: Q _ () () _
typedInsert = from usersTable # insert { name: "Alice", email: "alice@example.com", age: Nothing :: Maybe Int }

typedInsertOptional
  :: Q _ () () _
typedInsertOptional = from usersTable # insert { name: "Alice", email: "alice@example.com" }

typedInsertReturning
  :: Q _ (id :: Int, name :: String) () _
typedInsertReturning = from usersTable
  # insert { name: "Alice", email: "alice@example.com" }
  # returning @"id, name"

typedSet
  :: Q _ () () _
typedSet = from usersTable # set { name: "Bob" }

typedSetWhere
  :: Q _ () (id :: Int) _
typedSetWhere = from usersTable # set { name: "Bob" } # where_ @"id = $id"

typedSetReturning
  :: Q _ (id :: Int, name :: String, email :: String) (id :: Int) _
typedSetReturning = from usersTable
  # set { name: "Bob" }
  # where_ @"id = $id"
  # returning @"id, name, email"

typedUpsert
  :: Q _ () () _
typedUpsert = from usersTable
  # insert { name: "Alice", email: "alice@example.com", age: Nothing :: Maybe Int }
  # onConflictDoNothing @"email"

builderInsert :: String
builderInsert = typedInsert # toSQL

builderInsertReturning :: String
builderInsertReturning = typedInsertReturning # toSQL

builderSet :: String
builderSet = typedSet # toSQL

builderSetWhere :: String
builderSetWhere = typedSetWhere # toSQL

builderUpsert :: String
builderUpsert = typedUpsert # toSQL

typedDelete
  :: Q _ () (id :: Int) _
typedDelete = from usersTable # delete # where_ @"id = $id"

typedDeleteReturning
  :: Q _ (name :: String, email :: String) (id :: Int) _
typedDeleteReturning = from usersTable # delete # where_ @"id = $id" # returning @"name, email"

typedOrderBy
  :: Q _ (age :: Maybe Int, email :: String, id :: Int, name :: String) () _
typedOrderBy = from usersTable # selectAll # orderBy @"name"

typedOrderByDesc
  :: Q _ (age :: Maybe Int, email :: String, id :: Int, name :: String) () _
typedOrderByDesc = from usersTable # selectAll # orderBy @"name DESC, age ASC"

typedLimitOffset
  :: Q _ (age :: Maybe Int, email :: String, id :: Int, name :: String) () _
typedLimitOffset = from usersTable # selectAll # orderBy @"name" # limit @"10" # offset @"5"

typedInArray
  :: Q _ (age :: Maybe Int, email :: String, id :: Int, name :: String) (id :: Array Int) _
typedInArray = from usersTable # selectAll # where_ @"id IN $id"

typedReturningAll
  :: Q _ (age :: Maybe Int, email :: String, id :: Int, name :: String) (id :: Int) _
typedReturningAll = from usersTable # delete # where_ @"id = $id" # returningAll

builderDelete :: String
builderDelete = typedDelete # toSQL

builderDeleteReturning :: String
builderDeleteReturning = typedDeleteReturning # toSQL

builderOrderBy :: String
builderOrderBy = typedOrderBy # toSQL

builderOrderByDesc :: String
builderOrderByDesc = typedOrderByDesc # toSQL

builderLimitOffset :: String
builderLimitOffset = typedLimitOffset # toSQL

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Phase 10: Query execution (type annotations prove correctness)
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

queryAllUsers :: PG.Connection -> Aff (Array { id :: Int, name :: String, email :: String, age :: Maybe Int })
queryAllUsers conn = from usersTable # selectAll # runQuery conn {}

queryUsersByAge :: PG.Connection -> Aff (Array { name :: String, email :: String })
queryUsersByAge conn =
  from usersTable
    # select @"name, email"
    # where_ @"age > $age"
    # runQuery conn { age: 25 }

queryUserById :: PG.Connection -> Aff (Maybe { name :: String, e :: String })
queryUserById conn =
  from usersTable # select @"name, email AS e" # where_ @"id = $id" # runQueryOne conn { id: 1 }

queryComplexWhere :: PG.Connection -> Aff (Array { id :: Int, name :: String, email :: String, age :: Maybe Int })
queryComplexWhere conn =
  from usersTable # selectAll
    # where_ @"name = $name AND age > $age"
    # runQuery conn { name: "Alice", age: 21 }

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Phase 11: JOIN builder
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

type PostsTable = Table "posts"
  ( id :: Int # PrimaryKey # AutoIncrement
  , title :: String
  , body :: String
  , user_id :: Int
  )

postsTable :: Proxy PostsTable
postsTable = Proxy

type CommentsTable = Table "comments"
  ( id :: Int # PrimaryKey # AutoIncrement
  , text :: String
  , post_id :: Int
  , user_id :: Int
  )

commentsTable :: Proxy CommentsTable
commentsTable = Proxy

typedInnerJoin
  :: Q _ (name :: String, title :: String) (age :: Int) _
typedInnerJoin = from usersTable
  # innerJoin @"users.id = posts.user_id" postsTable
  # select @"users.name, posts.title"
  # where_ @"users.age > $age"

typedInnerJoinSQL :: String
typedInnerJoinSQL = typedInnerJoin # toSQL

typedInnerJoinAlias
  :: Q _ (user_name :: String, post_title :: String) () _
typedInnerJoinAlias = from usersTable
  # innerJoin @"users.id = posts.user_id" postsTable
  # select @"users.name AS user_name, posts.title AS post_title"

typedLeftJoin
  :: Q _ (name :: String, title :: Maybe String) () _
typedLeftJoin = from usersTable
  # leftJoin @"users.id = posts.user_id" postsTable
  # select @"users.name, posts.title"

typedLeftJoinSQL :: String
typedLeftJoinSQL = typedLeftJoin # toSQL

typedJoinOrderBy
  :: Q _ (name :: String, title :: String) () _
typedJoinOrderBy = from usersTable
  # innerJoin @"users.id = posts.user_id" postsTable
  # select @"users.name, posts.title"
  # orderBy @"users.name ASC"

typedJoinLimitOffset
  :: Q _ (name :: String, title :: String) () _
typedJoinLimitOffset = from usersTable
  # innerJoin @"users.id = posts.user_id" postsTable
  # select @"users.name, posts.title"
  # orderBy @"users.name"
  # limit @"10"
  # offset @"5"

typedUnqualifiedSelect
  :: Q _ (title :: String, name :: String) () _
typedUnqualifiedSelect = from usersTable
  # innerJoin @"users.id = posts.user_id" postsTable
  # select @"title, name"

typedMultiJoin
  :: Q _ (name :: String, title :: String, text :: String) () _
typedMultiJoin = from usersTable
  # innerJoin @"users.id = posts.user_id" postsTable
  # innerJoin @"posts.id = comments.post_id" commentsTable
  # select @"users.name, posts.title, comments.text"

joinQueryExecution :: PG.Connection -> Aff (Array { name :: String, title :: String })
joinQueryExecution conn =
  from usersTable
    # innerJoin @"users.id = posts.user_id" postsTable
    # select @"users.name, posts.title"
    # where_ @"users.age > $age"
    # runQuery conn { age: 25 }

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- DISTINCT
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

typedDistinct
  :: Q _ (name :: String) () _
typedDistinct = from usersTable # selectDistinct @"name"

typedDistinctOn
  :: Q _ (name :: String, email :: String) () _
typedDistinctOn = from usersTable # selectDistinctOn @"name" @"name, email"

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- GROUP BY, HAVING, Aggregates
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

typedGroupBy
  :: Q _ (name :: String) () _
typedGroupBy = from usersTable # select @"name" # groupBy @"name"

typedCountStar
  :: Q _ (name :: String, cnt :: Int) () _
typedCountStar = from usersTable
  # select @"name, COUNT(*) AS cnt"
  # groupBy @"name"

typedSumAge
  :: Q _ (name :: String, total :: Int) () _
typedSumAge = from usersTable
  # select @"name, SUM(age) AS total"
  # groupBy @"name"

typedAvgAge
  :: Q _ (name :: String, avg_age :: Number) () _
typedAvgAge = from usersTable
  # select @"name, AVG(age) AS avg_age"
  # groupBy @"name"

typedMinMax
  :: Q _ (name :: String, youngest :: Int, oldest :: Int) () _
typedMinMax = from usersTable
  # select @"name, MIN(age) AS youngest, MAX(age) AS oldest"
  # groupBy @"name"

typedHaving
  :: Q _ (name :: String, cnt :: Int) (minCount :: Int) _
typedHaving = from usersTable
  # select @"name, COUNT(*) AS cnt"
  # groupBy @"name"
  # having @"COUNT(*) > $minCount"

typedFullAggregate
  :: Q _ (name :: String, cnt :: Int) (min :: Int) _
typedFullAggregate = from usersTable
  # select @"name, COUNT(*) AS cnt"
  # groupBy @"name"
  # having @"COUNT(*) > $min"
  # orderBy @"name"
  # limit @"10"

typedWhereGroupByHaving
  :: Q _ (name :: String, cnt :: Int) (age :: Int, minCount :: Int) _
typedWhereGroupByHaving = from usersTable
  # select @"name, COUNT(*) AS cnt"
  # where_ @"age > $age"
  # groupBy @"name"
  # having @"COUNT(*) > $minCount"

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Table aliases
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

typedFromAs
  :: Q _ (name :: String) () _
typedFromAs = fromAs @"u" usersTable # select @"u.name"

typedSelfJoin
  :: Q _ (name :: String, manager_name :: String) () _
typedSelfJoin = from usersTable
  # innerJoinAs @"managers" @"users.id = managers.id" usersTable
  # select @"users.name, managers.name AS manager_name"

typedBothAliased
  :: Q _ (name :: String, other_name :: String) () _
typedBothAliased = fromAs @"u1" usersTable
  # innerJoinAs @"u2" @"u1.id = u2.id" usersTable
  # select @"u1.name, u2.name AS other_name"

typedLeftJoinAs
  :: Q _ (name :: String, other_email :: Maybe String) () _
typedLeftJoinAs = from usersTable
  # leftJoinAs @"u2" @"users.id = u2.id" usersTable
  # select @"users.name, u2.email AS other_email"

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Window functions
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

typedRowNumber
  :: Q _ (name :: String, rn :: Int) () _
typedRowNumber = from usersTable
  # select @"name, ROW_NUMBER() OVER (ORDER BY age) AS rn"

typedRank
  :: Q _ (name :: String, rnk :: Int) () _
typedRank = from usersTable
  # select @"name, RANK() OVER (PARTITION BY name ORDER BY age DESC) AS rnk"

typedDenseRank
  :: Q _ (name :: String, dr :: Int) () _
typedDenseRank = from usersTable
  # select @"name, DENSE_RANK() OVER (ORDER BY age) AS dr"

typedLag
  :: Q _ (name :: String, prev_age :: Int) () _
typedLag = from usersTable
  # select @"name, LAG(age) OVER (ORDER BY id) AS prev_age"

typedLead
  :: Q _ (name :: String, next_age :: Int) () _
typedLead = from usersTable
  # select @"name, LEAD(age) OVER (ORDER BY id) AS next_age"

typedFirstValue
  :: Q _ (name :: String, first_name :: String) () _
typedFirstValue = from usersTable
  # select @"name, FIRST_VALUE(name) OVER (ORDER BY age) AS first_name"

typedNtile
  :: Q _ (name :: String, bucket :: Int) () _
typedNtile = from usersTable
  # select @"name, NTILE(4) OVER (ORDER BY age) AS bucket"

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Parameterized LIMIT / OFFSET
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

typedLimitParam
  :: Q _ (name :: String) (n :: Int) _
typedLimitParam = from usersTable # select @"name" # limit @"$n"

typedOffsetParam
  :: Q _ (name :: String) (n :: Int) _
typedOffsetParam = from usersTable # select @"name" # offset @"$n"

typedLimitParamWithWhere
  :: Q _ (name :: String) (age :: Int, n :: Int) _
typedLimitParamWithWhere = from usersTable
  # select @"name"
  # where_ @"age > $age"
  # limit @"$n"

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- UNION / INTERSECT / EXCEPT
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

typedUnion
  :: Q _ (name :: String) () _
typedUnion = (from usersTable # select @"name")
  `union` (from usersTable # select @"name")

typedUnionAll
  :: Q _ (name :: String) () _
typedUnionAll = (from usersTable # select @"name")
  `unionAll` (from usersTable # select @"name")

typedUnionWithParams
  :: Q _ (name :: String) (a :: Int, b :: Int) _
typedUnionWithParams =
  (from usersTable # select @"name" # where_ @"age > $a")
    `union` (from usersTable # select @"name" # where_ @"age < $b")

typedUnionOrderByLimit
  :: Q _ (name :: String) () _
typedUnionOrderByLimit =
  (from usersTable # select @"name")
    `union` (from usersTable # select @"name")
    # orderBy @"name"
    # limit @"10"

typedIntersect
  :: Q _ (name :: String) () _
typedIntersect = (from usersTable # select @"name")
  `intersect` (from usersTable # select @"name")

typedExcept
  :: Q _ (name :: String) () _
typedExcept = (from usersTable # select @"name")
  `except_` (from usersTable # select @"name")

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- WHERE features
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

typedBetween
  :: Q _ _ (lo :: Int, hi :: Int) _
typedBetween = from usersTable # selectAll # where_ @"age BETWEEN $lo AND $hi"

typedIsNull
  :: Q _ _ () _
typedIsNull = from usersTable # selectAll # where_ @"age IS NULL"

typedIsNotNull
  :: Q _ _ () _
typedIsNotNull = from usersTable # selectAll # where_ @"age IS NOT NULL"

typedIsNullLowercase
  :: Q _ _ () _
typedIsNullLowercase = from usersTable # selectAll # where_ @"age is null"

typedBetweenLowercase
  :: Q _ _ (lo :: Int, hi :: Int) _
typedBetweenLowercase = from usersTable # selectAll # where_ @"age between $lo and $hi"

typedLikeLowercase
  :: Q _ _ (pat :: String) _
typedLikeLowercase = from usersTable # selectAll # where_ @"name like $pat"

typedNestedParens
  :: Q _ _ (a :: Int, b :: Int, name :: String) _
typedNestedParens = from usersTable # selectAll
  # where_ @"(age > $a OR age < $b) AND name = $name"

typedCoalesceSelect
  :: Q _ (name :: String, val :: Int) () _
typedCoalesceSelect = from usersTable
  # select @"name, COALESCE(age, 0) AS val"

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- UUID + DefaultExpr
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

type SessionsTable = Table "sessions"
  ( id :: PGUUID # PrimaryKey # DefaultExpr "gen_random_uuid()"
  , user_id :: Int # ForeignKey "users" References "id"
  , token :: String # Unique
  , created_at :: DateTime # DefaultExpr "now()"
  )

sessionsTable :: Proxy SessionsTable
sessionsTable = Proxy

sessionsDDL :: String
sessionsDDL = createTableDDL @SessionsTable

sessionsInsertSQL :: String
sessionsInsertSQL = insertSQLFor @SessionsTable

typedSessionSelect
  :: Q _ (id :: PGUUID, token :: String) () _
typedSessionSelect = from sessionsTable # select @"id, token"

typedSessionWhere
  :: Q _ (id :: PGUUID, token :: String, user_id :: Int, created_at :: DateTime) (userId :: Int) _
typedSessionWhere = from sessionsTable # selectAll # where_ @"user_id = $userId"

typedSessionInsert
  :: Q _ () () _
typedSessionInsert = from sessionsTable # insert { user_id: 42, token: "abc" }

builderSessionSelect :: String
builderSessionSelect = typedSessionSelect # toSQL

builderSessionInsert :: String
builderSessionInsert = typedSessionInsert # toSQL

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- PGDate + PGTime
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

type ScheduleTable = Table "schedule"
  ( id :: Int # PrimaryKey # AutoIncrement
  , event_date :: PGDate
  , start_time :: PGTime
  , title :: String
  )

scheduleTable :: Proxy ScheduleTable
scheduleTable = Proxy

scheduleDDL :: String
scheduleDDL = createTableDDL @ScheduleTable

typedScheduleSelect
  :: Q _ (event_date :: PGDate, start_time :: PGTime, title :: String) () _
typedScheduleSelect = from scheduleTable # select @"event_date, start_time, title"

typedScheduleWhere
  :: Q _ _ (d :: PGDate) _
typedScheduleWhere = from scheduleTable # selectAll # where_ @"event_date = $d"

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Spec
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

spec :: Spec Unit
spec = do
  describe "Schema" do
    describe "CREATE TABLE DDL" do
      it "generates correct DDL for UsersTable" do
        ddl `shouldEqual` "CREATE TABLE users (age INTEGER, email TEXT NOT NULL UNIQUE, id INTEGER NOT NULL GENERATED ALWAYS AS IDENTITY PRIMARY KEY, name TEXT NOT NULL)"

    describe "Extended types DDL" do
      it "generates DDL with JSONB, TIMESTAMPTZ, BIGINT, arrays" do
        eventsDDL `shouldEqual` "CREATE TABLE events (created_at TIMESTAMPTZ NOT NULL, id INTEGER NOT NULL GENERATED ALWAYS AS IDENTITY PRIMARY KEY, metadata JSONB NOT NULL, tags TEXT[] NOT NULL, title TEXT NOT NULL, view_count BIGINT NOT NULL)"

    describe "INSERT SQL" do
      it "generates INSERT skipping AutoIncrement columns" do
        insertSQL `shouldEqual` "INSERT INTO users (age, email, name) VALUES ($1, $2, $3) RETURNING *"

    describe "SELECT SQL" do
      it "generates SELECT ALL" do
        selectAllSQL `shouldEqual` "SELECT * FROM users"
      it "generates SELECT WHERE" do
        selectWhereSQL `shouldEqual` "SELECT * FROM users WHERE id = $1"

    describe "Defaults" do
      it "renders DEFAULT in DDL" do
        configDDL `shouldEqual` "CREATE TABLE config (active BOOLEAN NOT NULL DEFAULT true, id INTEGER NOT NULL GENERATED ALWAYS AS IDENTITY PRIMARY KEY, role TEXT NOT NULL DEFAULT 'user', score INTEGER NOT NULL DEFAULT 0)"
      it "skips Default columns in INSERT" do
        configInsertSQL `shouldEqual` "INSERT INTO config () VALUES () RETURNING *"

    describe "UUID + DefaultExpr" do
      it "renders UUID and DefaultExpr in DDL" do
        sessionsDDL `shouldEqual` "CREATE TABLE sessions (created_at TIMESTAMPTZ NOT NULL DEFAULT now(), id UUID NOT NULL DEFAULT gen_random_uuid() PRIMARY KEY, token TEXT NOT NULL UNIQUE, user_id INTEGER NOT NULL REFERENCES users(id))"
      it "skips DefaultExpr columns in INSERT" do
        sessionsInsertSQL `shouldEqual` "INSERT INTO sessions (token, user_id) VALUES ($1, $2) RETURNING *"
      it "builds SELECT with UUID column" do
        builderSessionSelect `shouldEqual` "SELECT id, token FROM sessions"
      it "builds INSERT skipping DefaultExpr columns" do
        builderSessionInsert `shouldEqual` "INSERT INTO sessions (token, user_id) VALUES ($1, $2)"

    describe "PGDate + PGTime" do
      it "renders DATE and TIME in DDL" do
        scheduleDDL `shouldEqual` "CREATE TABLE schedule (event_date DATE NOT NULL, id INTEGER NOT NULL GENERATED ALWAYS AS IDENTITY PRIMARY KEY, start_time TIME NOT NULL, title TEXT NOT NULL)"

    describe "UPDATE SQL" do
      it "generates UPDATE with SET and WHERE" do
        updateSQL `shouldEqual` "UPDATE users SET name = $1 WHERE id = $2"

    describe "DELETE SQL" do
      it "generates DELETE with WHERE" do
        deleteSQL `shouldEqual` "DELETE FROM users WHERE id = $1"

    describe "Builder API" do
      it "builds SELECT *" do
        builderSelectAll `shouldEqual` "SELECT * FROM users"
      it "builds SELECT with columns" do
        builderSelectCols `shouldEqual` "SELECT name, email FROM users"
      it "builds SELECT * with WHERE" do
        builderSelectWhere `shouldEqual` "SELECT * FROM users WHERE id = $id"
      it "builds SELECT columns with WHERE" do
        builderSelectColsWhere `shouldEqual` "SELECT name FROM users WHERE age > $age"
      it "builds SELECT with aliases" do
        builderSelectAlias `shouldEqual` "SELECT name, email AS e FROM users"
      it "builds complex WHERE" do
        builderWhereComplex `shouldEqual` "SELECT * FROM users WHERE name = $name AND age > $age"

    describe "Builder INSERT" do
      it "builds INSERT" do
        builderInsert `shouldEqual` "INSERT INTO users (age, email, name) VALUES ($1, $2, $3)"
      it "builds INSERT with RETURNING" do
        builderInsertReturning `shouldEqual` "INSERT INTO users (email, name) VALUES ($1, $2) RETURNING id, name"

    describe "Builder UPDATE" do
      it "builds UPDATE SET" do
        builderSet `shouldEqual` "UPDATE users SET name = $1"
      it "builds UPDATE SET with WHERE" do
        builderSetWhere `shouldEqual` "UPDATE users SET name = $1 WHERE id = $id"

    describe "Builder ON CONFLICT" do
      it "builds INSERT ON CONFLICT DO NOTHING" do
        builderUpsert `shouldEqual` "INSERT INTO users (age, email, name) VALUES ($1, $2, $3) ON CONFLICT (email) DO NOTHING"

    describe "Builder DELETE" do
      it "builds DELETE with WHERE" do
        builderDelete `shouldEqual` "DELETE FROM users WHERE id = $id"
      it "builds DELETE with RETURNING" do
        builderDeleteReturning `shouldEqual` "DELETE FROM users WHERE id = $id RETURNING name, email"

    describe "Builder ORDER BY / LIMIT / OFFSET" do
      it "builds ORDER BY" do
        builderOrderBy `shouldEqual` "SELECT * FROM users ORDER BY name"
      it "builds ORDER BY with DESC/ASC" do
        builderOrderByDesc `shouldEqual` "SELECT * FROM users ORDER BY name DESC, age ASC"
      it "builds LIMIT and OFFSET" do
        builderLimitOffset `shouldEqual` "SELECT * FROM users ORDER BY name LIMIT 10 OFFSET 5"

    describe "Builder JOIN" do
      it "builds INNER JOIN with SELECT and WHERE" do
        typedInnerJoinSQL `shouldEqual` "SELECT users.name, posts.title FROM users INNER JOIN posts ON users.id = posts.user_id WHERE users.age > $age"
      it "builds LEFT JOIN" do
        typedLeftJoinSQL `shouldEqual` "SELECT users.name, posts.title FROM users LEFT JOIN posts ON users.id = posts.user_id"
      it "builds JOIN with ORDER BY, LIMIT, OFFSET" do
        (typedJoinLimitOffset # toSQL) `shouldEqual` "SELECT users.name, posts.title FROM users INNER JOIN posts ON users.id = posts.user_id ORDER BY users.name LIMIT 10 OFFSET 5"
      it "builds multi-way JOIN" do
        (typedMultiJoin # toSQL) `shouldEqual` "SELECT users.name, posts.title, comments.text FROM users INNER JOIN posts ON users.id = posts.user_id INNER JOIN comments ON posts.id = comments.post_id"

    describe "Builder table aliases" do
      it "builds FROM with alias" do
        (typedFromAs # toSQL) `shouldEqual` "SELECT u.name FROM users u"
      it "builds self-join with innerJoinAs" do
        (typedSelfJoin # toSQL) `shouldEqual` "SELECT users.name, managers.name AS manager_name FROM users INNER JOIN users managers ON users.id = managers.id"
      it "builds both sides aliased" do
        (typedBothAliased # toSQL) `shouldEqual` "SELECT u1.name, u2.name AS other_name FROM users u1 INNER JOIN users u2 ON u1.id = u2.id"
      it "builds LEFT JOIN with alias" do
        (typedLeftJoinAs # toSQL) `shouldEqual` "SELECT users.name, u2.email AS other_email FROM users LEFT JOIN users u2 ON users.id = u2.id"

    describe "Builder DISTINCT" do
      it "builds SELECT DISTINCT" do
        (typedDistinct # toSQL) `shouldEqual` "SELECT DISTINCT name FROM users"
      it "builds SELECT DISTINCT ON" do
        (typedDistinctOn # toSQL) `shouldEqual` "SELECT DISTINCT ON (name) name, email FROM users"

    describe "Builder GROUP BY, HAVING, Aggregates" do
      it "builds GROUP BY" do
        (typedGroupBy # toSQL) `shouldEqual` "SELECT name FROM users GROUP BY name"
      it "builds COUNT(*)" do
        (typedCountStar # toSQL) `shouldEqual` "SELECT name, COUNT(*) AS cnt FROM users GROUP BY name"
      it "builds SUM(col)" do
        (typedSumAge # toSQL) `shouldEqual` "SELECT name, SUM(age) AS total FROM users GROUP BY name"
      it "builds AVG(col)" do
        (typedAvgAge # toSQL) `shouldEqual` "SELECT name, AVG(age) AS avg_age FROM users GROUP BY name"
      it "builds MIN/MAX" do
        (typedMinMax # toSQL) `shouldEqual` "SELECT name, MIN(age) AS youngest, MAX(age) AS oldest FROM users GROUP BY name"
      it "builds HAVING" do
        (typedHaving # toSQL) `shouldEqual` "SELECT name, COUNT(*) AS cnt FROM users GROUP BY name HAVING COUNT(*) > $minCount"
      it "builds full aggregate chain" do
        (typedFullAggregate # toSQL) `shouldEqual` "SELECT name, COUNT(*) AS cnt FROM users GROUP BY name HAVING COUNT(*) > $min ORDER BY name LIMIT 10"
      it "builds WHERE + GROUP BY + HAVING" do
        (typedWhereGroupByHaving # toSQL) `shouldEqual` "SELECT name, COUNT(*) AS cnt FROM users WHERE age > $age GROUP BY name HAVING COUNT(*) > $minCount"
      it "builds COALESCE with nested parens" do
        (typedCoalesceSelect # toSQL) `shouldEqual` "SELECT name, COALESCE(age, 0) AS val FROM users"

    describe "Builder window functions" do
      it "builds ROW_NUMBER() OVER" do
        (typedRowNumber # toSQL) `shouldEqual` "SELECT name, ROW_NUMBER() OVER (ORDER BY age) AS rn FROM users"
      it "builds RANK() OVER with PARTITION BY" do
        (typedRank # toSQL) `shouldEqual` "SELECT name, RANK() OVER (PARTITION BY name ORDER BY age DESC) AS rnk FROM users"
      it "builds DENSE_RANK() OVER" do
        (typedDenseRank # toSQL) `shouldEqual` "SELECT name, DENSE_RANK() OVER (ORDER BY age) AS dr FROM users"
      it "builds LAG(col) OVER" do
        (typedLag # toSQL) `shouldEqual` "SELECT name, LAG(age) OVER (ORDER BY id) AS prev_age FROM users"
      it "builds LEAD(col) OVER" do
        (typedLead # toSQL) `shouldEqual` "SELECT name, LEAD(age) OVER (ORDER BY id) AS next_age FROM users"
      it "builds FIRST_VALUE(col) OVER" do
        (typedFirstValue # toSQL) `shouldEqual` "SELECT name, FIRST_VALUE(name) OVER (ORDER BY age) AS first_name FROM users"
      it "builds NTILE(n) OVER" do
        (typedNtile # toSQL) `shouldEqual` "SELECT name, NTILE(4) OVER (ORDER BY age) AS bucket FROM users"

    describe "Builder parameterized LIMIT/OFFSET" do
      it "builds LIMIT with param" do
        (typedLimitParam # toSQL) `shouldEqual` "SELECT name FROM users LIMIT $n"
      it "builds OFFSET with param" do
        (typedOffsetParam # toSQL) `shouldEqual` "SELECT name FROM users OFFSET $n"
      it "combines WHERE params with LIMIT param" do
        (typedLimitParamWithWhere # toSQL) `shouldEqual` "SELECT name FROM users WHERE age > $age LIMIT $n"

    describe "Builder UNION / INTERSECT / EXCEPT" do
      it "builds UNION" do
        (typedUnion # toSQL) `shouldEqual` "(SELECT name FROM users) UNION (SELECT name FROM users)"
      it "builds UNION ALL" do
        (typedUnionAll # toSQL) `shouldEqual` "(SELECT name FROM users) UNION ALL (SELECT name FROM users)"
      it "builds UNION with params from both sides" do
        (typedUnionWithParams # toSQL) `shouldEqual` "(SELECT name FROM users WHERE age > $a) UNION (SELECT name FROM users WHERE age < $b)"
      it "builds UNION with ORDER BY and LIMIT" do
        (typedUnionOrderByLimit # toSQL) `shouldEqual` "(SELECT name FROM users) UNION (SELECT name FROM users) ORDER BY name LIMIT 10"
      it "builds INTERSECT" do
        (typedIntersect # toSQL) `shouldEqual` "(SELECT name FROM users) INTERSECT (SELECT name FROM users)"
      it "builds EXCEPT" do
        (typedExcept # toSQL) `shouldEqual` "(SELECT name FROM users) EXCEPT (SELECT name FROM users)"

    describe "Builder WHERE features" do
      it "builds BETWEEN" do
        (typedBetween # toSQL) `shouldEqual` "SELECT * FROM users WHERE age BETWEEN $lo AND $hi"
      it "builds IS NULL" do
        (typedIsNull # toSQL) `shouldEqual` "SELECT * FROM users WHERE age IS NULL"
      it "builds IS NOT NULL" do
        (typedIsNotNull # toSQL) `shouldEqual` "SELECT * FROM users WHERE age IS NOT NULL"
      it "builds lowercase is null" do
        (typedIsNullLowercase # toSQL) `shouldEqual` "SELECT * FROM users WHERE age is null"
      it "builds lowercase between" do
        (typedBetweenLowercase # toSQL) `shouldEqual` "SELECT * FROM users WHERE age between $lo and $hi"
      it "builds lowercase like" do
        (typedLikeLowercase # toSQL) `shouldEqual` "SELECT * FROM users WHERE name like $pat"
      it "builds nested parentheses" do
        (typedNestedParens # toSQL) `shouldEqual` "SELECT * FROM users WHERE (age > $a OR age < $b) AND name = $name"

resetUsers :: PG.Connection -> Aff Unit
resetUsers conn = do
  _ <- PG.executeSimple (PG.SQL "DELETE FROM users") conn
  _ <- PG.executeSimple (PG.SQL "ALTER SEQUENCE users_id_seq RESTART WITH 1") conn
  _ <- PG.execute (PG.SQL "INSERT INTO users (name, email, age) VALUES ($1, $2, $3)")
    [ PG.toPGValue "Alice", PG.toPGValue "alice@example.com", PG.toPGValue 22 ]
    conn
  _ <- PG.execute (PG.SQL "INSERT INTO users (name, email, age) VALUES ($1, $2, $3)")
    [ PG.toPGValue "Bob", PG.toPGValue "bob@example.com", PG.toPGValue 30 ]
    conn
  pure unit

integrationSpec :: PG.Connection -> Spec Unit
integrationSpec conn = do
  describe "Builder query execution" do
    it "selectAll returns all rows" do
      resetUsers conn
      rows <- from usersTable # select @"name, email, age"
        # orderBy @"name"
        # runQuery conn {}
      rows `shouldEqual`
        [ { name: "Alice", email: "alice@example.com", age: Just 22 }
        , { name: "Bob", email: "bob@example.com", age: Just 30 }
        ]

    it "select with columns returns projected rows" do
      rows <- from usersTable
        # select @"name, email"
        # where_ @"age > $age"
        # runQuery conn { age: 25 }
      rows `shouldEqual` [ { name: "Bob", email: "bob@example.com" } ]

    it "select with alias" do
      rows <- from usersTable
        # select @"name, email AS e"
        # orderBy @"name"
        # runQuery conn {}
      rows `shouldEqual`
        [ { name: "Alice", e: "alice@example.com" }
        , { name: "Bob", e: "bob@example.com" }
        ]

    it "runQueryOne returns Just for match" do
      result <- from usersTable
        # select @"name, email"
        # where_ @"name = $name"
        # runQueryOne conn { name: "Alice" }
      result `shouldEqual` Just { name: "Alice", email: "alice@example.com" }

    it "runQueryOne returns Nothing for no match" do
      result <- from usersTable
        # selectAll
        # where_ @"name = $name"
        # runQueryOne conn { name: "Nobody" }
      result `shouldEqual` Nothing

  describe "Extended types execution" do
    it "inserts and selects Jsonb, Array, DateTime, BigInt" do
      _ <- from eventsTable
        # insert
            { title: "Launch"
            , metadata: Jsonb (unsafeToForeign { key: "value", nested: { a: 1 } })
            , tags: [ "release", "v1" ]
            , created_at: testDateTime
            , view_count: BigInt.fromInt 42
            }
        # runExecute conn {}
      rows <- from eventsTable
        # select @"title, tags, view_count"
        # where_ @"title = $title"
        # runQuery conn { title: "Launch" }
      rows `shouldEqual` [ { title: "Launch", tags: [ "release", "v1" ], view_count: BigInt.fromInt 42 } ]

    it "inserts with returningAll" do
      rows <- from eventsTable
        # insert { title: "Minimal", metadata: Jsonb (unsafeToForeign {}), tags: ([] :: Array String), created_at: testDateTime, view_count: BigInt.fromInt 0 }
        # returning @"title"
        # runQuery conn {}
      rows `shouldEqual` [ { title: "Minimal" } ]

    it "WHERE with JSONB @> operator" do
      rows <- from eventsTable
        # select @"title"
        # where_ @"metadata @> $metadata"
        # runQuery conn { metadata: Jsonb (unsafeToForeign { key: "value" }) }
      rows `shouldEqual` [ { title: "Launch" } ]

    it "WHERE with = ANY() array param" do
      rows <- from usersTable
        # select @"name"
        # where_ @"id = ANY($id)"
        # orderBy @"name"
        # runQuery conn { id: [ 1, 2 ] }
      rows `shouldEqual` [ { name: "Alice" }, { name: "Bob" } ]

    it "complex where with multiple params" do
      rows <- from usersTable
        # select @"name, age"
        # where_ @"name = $name AND age > $age"
        # runQuery conn { name: "Bob", age: 20 }
      rows `shouldEqual` [ { name: "Bob", age: Just 30 } ]

  describe "Builder insert execution" do
    it "inserts a row and returns count" do
      resetUsers conn
      count <- from usersTable
        # insert { name: "Charlie", email: "charlie@example.com", age: Just 28 }
        # runExecute conn {}
      count `shouldEqual` 1
      rows <- from usersTable
        # select @"name"
        # where_ @"name = $name"
        # runQuery conn { name: "Charlie" }
      rows `shouldEqual` [ { name: "Charlie" } ]

    it "inserts with RETURNING" do
      rows <- from usersTable
        # insert { name: "Diana", email: "diana@example.com" }
        # returning @"name, email"
        # runQuery conn {}
      rows `shouldEqual` [ { name: "Diana", email: "diana@example.com" } ]

  describe "Builder update execution" do
    it "updates a row" do
      resetUsers conn
      count <- from usersTable
        # set { email: "alice-updated@example.com" }
        # where_ @"name = $name"
        # runExecute conn { name: "Alice" }
      count `shouldEqual` 1
      result <- from usersTable
        # select @"email"
        # where_ @"name = $name"
        # runQueryOne conn { name: "Alice" }
      result `shouldEqual` Just { email: "alice-updated@example.com" }

    it "updates with RETURNING" do
      rows <- from usersTable
        # set { age: Just 99 }
        # where_ @"name = $name"
        # returning @"name, age"
        # runQuery conn { name: "Bob" }
      rows `shouldEqual` [ { name: "Bob", age: Just 99 } ]

  describe "Builder upsert execution" do
    it "ON CONFLICT DO NOTHING skips duplicate" do
      resetUsers conn
      count <- from usersTable
        # insert { name: "Duplicate", email: "bob@example.com" }
        # onConflictDoNothing @"email"
        # runExecute conn {}
      count `shouldEqual` 0

    it "ON CONFLICT DO UPDATE" do
      rows <- from usersTable
        # insert { name: "Updated", email: "bob@example.com", age: Just 40 }
        # onConflict @"email" @"DO UPDATE SET name = EXCLUDED.name, age = EXCLUDED.age"
        # returning @"name, age"
        # runQuery conn {}
      rows `shouldEqual` [ { name: "Updated", age: Just 40 } ]

  describe "Builder order by / limit / offset execution" do
    it "ORDER BY sorts results" do
      resetUsers conn
      rows <- from usersTable
        # select @"name"
        # orderBy @"name ASC"
        # runQuery conn {}
      rows `shouldEqual` [ { name: "Alice" }, { name: "Bob" } ]

    it "LIMIT restricts row count" do
      rows <- from usersTable
        # select @"name"
        # orderBy @"name"
        # limit @"1"
        # runQuery conn {}
      rows `shouldEqual` [ { name: "Alice" } ]

    it "OFFSET skips rows" do
      rows <- from usersTable
        # select @"name"
        # orderBy @"name ASC"
        # limit @"1"
        # offset @"1"
        # runQuery conn {}
      rows `shouldEqual` [ { name: "Bob" } ]

  describe "Builder delete execution" do
    it "deletes with RETURNING" do
      resetUsers conn
      _ <- from usersTable
        # insert { name: "ToDelete", email: "delete@example.com", age: Just 50 }
        # runExecute conn {}
      rows <- from usersTable
        # delete
        # where_ @"name = $name"
        # returning @"name, email"
        # runQuery conn { name: "ToDelete" }
      rows `shouldEqual` [ { name: "ToDelete", email: "delete@example.com" } ]
      result <- from usersTable
        # select @"name"
        # where_ @"name = $name"
        # runQueryOne conn { name: "ToDelete" }
      result `shouldEqual` Nothing

  describe "Builder JOIN execution" do
    it "INNER JOIN returns matching rows" do
      resetUsers conn
      rows <- from usersTable
        # innerJoin @"users.id = posts.user_id" postsTable
        # select @"users.name, posts.title"
        # orderBy @"users.name"
        # runQuery conn {}
      rows `shouldEqual`
        [ { name: "Alice", title: "Alice's Post" }
        , { name: "Bob", title: "Bob's Post" }
        ]

    it "INNER JOIN with WHERE filters correctly" do
      rows <- from usersTable
        # innerJoin @"users.id = posts.user_id" postsTable
        # select @"users.name, posts.title"
        # where_ @"users.age > $age"
        # runQuery conn { age: 25 }
      rows `shouldEqual` [ { name: "Bob", title: "Bob's Post" } ]

    it "INNER JOIN with aliases" do
      rows <- from usersTable
        # innerJoin @"users.id = posts.user_id" postsTable
        # select @"users.name AS author, posts.title AS post_title"
        # orderBy @"users.name"
        # runQuery conn {}
      rows `shouldEqual`
        [ { author: "Alice", post_title: "Alice's Post" }
        , { author: "Bob", post_title: "Bob's Post" }
        ]

    it "INNER JOIN with ORDER BY and LIMIT" do
      rows <- from usersTable
        # innerJoin @"users.id = posts.user_id" postsTable
        # select @"users.name, posts.title"
        # orderBy @"users.name ASC"
        # limit @"1"
        # runQuery conn {}
      rows `shouldEqual` [ { name: "Alice", title: "Alice's Post" } ]

    it "LEFT JOIN includes unmatched rows" do
      _ <- PG.execute (PG.SQL "INSERT INTO users (name, email, age) VALUES ($1, $2, $3)")
        [ PG.toPGValue "NoPost", PG.toPGValue "nopost@example.com", PG.toPGValue 40 ]
        conn
      rows <- from usersTable
        # leftJoin @"users.id = posts.user_id" postsTable
        # select @"users.name, posts.title"
        # orderBy @"users.name ASC"
        # runQuery conn {}
      rows `shouldEqual`
        [ { name: "Alice", title: Just "Alice's Post" }
        , { name: "Bob", title: Just "Bob's Post" }
        , { name: "NoPost", title: Nothing }
        ]
      _ <- PG.execute (PG.SQL "DELETE FROM users WHERE name = $1")
        [ PG.toPGValue "NoPost" ]
        conn
      pure unit

    it "runQueryOne returns Just for match" do
      result <- from usersTable
        # innerJoin @"users.id = posts.user_id" postsTable
        # select @"users.name, posts.title"
        # where_ @"users.name = $name"
        # runQueryOne conn { name: "Alice" }
      result `shouldEqual` Just { name: "Alice", title: "Alice's Post" }

  describe "Parameterized LIMIT/OFFSET execution" do
    it "parameterized LIMIT restricts rows" do
      resetUsers conn
      rows <- from usersTable # select @"name" # orderBy @"name"
        # limit @"$n"
        # runQuery conn { n: 1 }
      rows `shouldEqual` [ { name: "Alice" } ]

    it "parameterized OFFSET skips rows" do
      rows <- from usersTable # select @"name" # orderBy @"name"
        # limit @"$n"
        # offset @"$off"
        # runQuery conn { n: 1, off: 1 }
      rows `shouldEqual` [ { name: "Bob" } ]

    it "parameterized LIMIT with WHERE" do
      rows <- from usersTable # select @"name"
        # where_ @"age > $age"
        # orderBy @"name"
        # limit @"$n"
        # runQuery conn { age: 0, n: 1 }
      rows `shouldEqual` [ { name: "Alice" } ]

  describe "UNION / INTERSECT / EXCEPT execution" do
    it "UNION deduplicates" do
      resetUsers conn
      rows <-
        (from usersTable # select @"name")
          # union (from usersTable # select @"name")
          # orderBy @"name"
          # runQuery conn {}
      rows `shouldEqual` [ { name: "Alice" }, { name: "Bob" } ]

    it "UNION ALL keeps duplicates" do
      rows <-
        (from usersTable # select @"name")
          # unionAll (from usersTable # select @"name")
          # orderBy @"name"
          # runQuery conn {}
      rows `shouldEqual`
        [ { name: "Alice" }
        , { name: "Alice" }
        , { name: "Bob" }
        , { name: "Bob" }
        ]

    it "UNION with params from both sides" do
      rows <-
        (from usersTable # select @"name" # where_ @"age > $maxAge")
          # union (from usersTable # select @"name" # where_ @"age < $minAge")
          # runQuery conn { maxAge: 100, minAge: 18 }
      rows `shouldEqual` []

    it "INTERSECT returns common rows" do
      rows <-
        (from usersTable # select @"name")
          # intersect (from usersTable # select @"name")
          # orderBy @"name"
          # runQuery conn {}
      rows `shouldEqual` [ { name: "Alice" }, { name: "Bob" } ]

    it "EXCEPT removes matching rows" do
      rows <-
        (from usersTable # select @"name")
          `except_` (from usersTable # select @"name" # where_ @"name = $name")
          # runQuery conn { name: "Alice" }
      rows `shouldEqual` [ { name: "Bob" } ]

    it "UNION with ORDER BY and LIMIT" do
      rows <-
        (from usersTable # select @"name")
          `union` (from usersTable # select @"name")
          # orderBy @"name"
          # limit @"1"
          # runQuery conn {}
      rows `shouldEqual` [ { name: "Alice" } ]

  describe "WHERE features execution" do
    it "BETWEEN filters range" do
      resetUsers conn
      rows <- from usersTable # select @"name"
        # where_ @"age BETWEEN $lo AND $hi"
        # runQuery conn { lo: 20, hi: 26 }
      rows `shouldEqual` [ { name: "Alice" } ]

    it "IS NULL matches null values" do
      _ <- from usersTable
        # insert { name: "NullAge", email: "nullage@example.com" }
        # runExecute conn {}
      rows <- from usersTable # select @"name"
        # where_ @"age IS NULL"
        # runQuery conn {}
      rows `shouldEqual` [ { name: "NullAge" } ]
      _ <- PG.execute (PG.SQL "DELETE FROM users WHERE name = $1")
        [ PG.toPGValue "NullAge" ]
        conn
      pure unit

    it "IS NOT NULL excludes null values" do
      _ <- from usersTable
        # insert { name: "NullAge2", email: "nullage2@example.com" }
        # runExecute conn {}
      rows <- from usersTable # select @"name"
        # where_ @"age IS NOT NULL"
        # orderBy @"name"
        # runQuery conn {}
      rows `shouldEqual` [ { name: "Alice" }, { name: "Bob" } ]
      _ <- PG.execute (PG.SQL "DELETE FROM users WHERE name = $1")
        [ PG.toPGValue "NullAge2" ]
        conn
      pure unit

    it "nested parentheses" do
      rows <- from usersTable # select @"name"
        # where_ @"(age > $a OR age < $b) AND name = $name"
        # runQuery conn { a: 100, b: 0, name: "Alice" }
      rows `shouldEqual` ([] :: Array { name :: String })

  describe "HAVING with WHERE params" do
    it "WHERE + GROUP BY + HAVING merges params" do
      resetUsers conn
      rows <- from usersTable
        # select @"name"
        # where_ @"age > $age"
        # groupBy @"name"
        # having @"COUNT(*) > $minCount"
        # orderBy @"name"
        # runQuery conn { age: 10, minCount: 0 }
      rows `shouldEqual` [ { name: "Alice" }, { name: "Bob" } ]
