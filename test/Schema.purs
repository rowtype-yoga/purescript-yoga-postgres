module Test.Postgres.Schema where

import Prelude

import Data.Array as Array
import Data.DateTime (DateTime)
import Data.Maybe (Maybe(..))
import Data.String (contains, Pattern(..))
import Data.Tuple.Nested (type (/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign (Foreign)
import JS.BigInt (BigInt)
import Prim.Boolean (True)
import Test.Spec (Spec, around, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Type.Proxy (Proxy(..))
import Yoga.Postgres as PG
import Yoga.Postgres.Schema

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Phase 1: Table type definitions compile
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

type UsersTable = Table "users"
  ( id :: Column Int (PrimaryKey /\ AutoIncrement)
  , name :: Column String None
  , email :: Column String Unique
  , age :: Column (Maybe Int) None
  )

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Phase 2: CREATE TABLE DDL generation
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

ddl :: String
ddl = createTableDDL @UsersTable

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Phase 3: Type-safe INSERT
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- Insert should require non-PK, non-default columns
-- PK columns (id) are omitted
-- Maybe columns (age) become optional

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
  ( id :: Column Int (PrimaryKey /\ AutoIncrement)
  , active :: Column Boolean (DefaultBool True)
  , role :: Column String (Default "'user'")
  , score :: Column Int (DefaultInt 0)
  )

configDDL :: String
configDDL = createTableDDL @ConfigTable

configInsertSQL :: String
configInsertSQL = insertSQLFor @ConfigTable

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Extended types: DateTime, BigInt, Jsonb, Array
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

type EventsTable = Table "events"
  ( id :: Column Int (PrimaryKey /\ AutoIncrement)
  , title :: Column String None
  , metadata :: Column Jsonb None
  , tags :: Column (Array String) None
  , created_at :: Column DateTime None
  , view_count :: Column BigInt None
  )

eventsTable :: Proxy EventsTable
eventsTable = Proxy

eventsDDL :: String
eventsDDL = createTableDDL @EventsTable

typedJsonbWhere
  :: Q "events" _ _ (metadata :: Jsonb) _
typedJsonbWhere = from eventsTable # selectAll # where_ @"metadata @> $metadata"

typedTitleLike
  :: Q "events" _ _ (title :: String) _
typedTitleLike = from eventsTable # selectAll # where_ @"title LIKE $title"

typedArrayWhere
  :: Q "events" _ _ (id :: Array Int) _
typedArrayWhere = from eventsTable # selectAll # where_ @"id IN $id"

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

-- Type annotations prove the compiler tracks result and param types

typedSelectAll
  :: Q "users" _
       (age :: Maybe Int, email :: String, id :: Int, name :: String)
       ()
       _
typedSelectAll = from usersTable # selectAll

typedSelectCols
  :: Q "users" _
       (name :: String, email :: String)
       ()
       _
typedSelectCols = from usersTable # select @"name, email"

typedSelectAlias
  :: Q "users" _
       (name :: String, e :: String)
       ()
       _
typedSelectAlias = from usersTable # select @"name, email AS e"

typedWhere
  :: Q "users" _
       (age :: Maybe Int, email :: String, id :: Int, name :: String)
       (id :: Int)
       _
typedWhere = from usersTable # selectAll # where_ @"id = $id"

typedWhereComplex
  :: Q "users" _
       (age :: Maybe Int, email :: String, id :: Int, name :: String)
       (name :: String, age :: Int)
       _
typedWhereComplex = from usersTable # selectAll # where_ @"name = $name AND age > $age"

typedSelectColsWhere
  :: Q "users" _
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
  :: Q "users" _ () () _
typedInsert = from usersTable # insert { name: "Alice", email: "alice@example.com", age: Nothing :: Maybe Int }

typedInsertOptional
  :: Q "users" _ () () _
typedInsertOptional = from usersTable # insert { name: "Alice", email: "alice@example.com" }

typedInsertReturning
  :: Q "users" _ (id :: Int, name :: String) () _
typedInsertReturning = from usersTable
  # insert { name: "Alice", email: "alice@example.com" }
  # returning @"id, name"

typedSet
  :: Q "users" _ () () _
typedSet = from usersTable # set { name: "Bob" }

typedSetWhere
  :: Q "users" _ () (id :: Int) _
typedSetWhere = from usersTable # set { name: "Bob" } # where_ @"id = $id"

typedSetReturning
  :: Q "users" _ (id :: Int, name :: String, email :: String) (id :: Int) _
typedSetReturning = from usersTable
  # set { name: "Bob" }
  # where_ @"id = $id"
  # returning @"id, name, email"

typedUpsert
  :: Q "users" _ () () _
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
  :: Q "users" _ () (id :: Int) _
typedDelete = from usersTable # delete # where_ @"id = $id"

typedDeleteReturning
  :: Q "users" _ (name :: String, email :: String) (id :: Int) _
typedDeleteReturning = from usersTable # delete # where_ @"id = $id" # returning @"name, email"

typedOrderBy
  :: Q "users" _ (age :: Maybe Int, email :: String, id :: Int, name :: String) () _
typedOrderBy = from usersTable # selectAll # orderBy @"name"

typedOrderByDesc
  :: Q "users" _ (age :: Maybe Int, email :: String, id :: Int, name :: String) () _
typedOrderByDesc = from usersTable # selectAll # orderBy @"name DESC, age ASC"

typedLimitOffset
  :: Q "users" _ (age :: Maybe Int, email :: String, id :: Int, name :: String) () _
typedLimitOffset = from usersTable # selectAll # orderBy @"name" # limit 10 # offset 5

typedInArray
  :: Q "users" _ (age :: Maybe Int, email :: String, id :: Int, name :: String) (id :: Array Int) _
typedInArray = from usersTable # selectAll # where_ @"id IN $id"

typedReturningAll
  :: Q "users" _ (age :: Maybe Int, email :: String, id :: Int, name :: String) (id :: Int) _
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
  from usersTable # selectAll # where_ @"name = $name AND age > $age" # runQuery conn { name: "Alice", age: 21 }

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Spec
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

spec :: Spec Unit
spec = do
  describe "Schema" do
    describe "CREATE TABLE DDL" do
      it "generates correct DDL for UsersTable" do
        ddl `shouldSatisfy` contains (Pattern "CREATE TABLE users")
        ddl `shouldSatisfy` contains (Pattern "id INTEGER NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY")
        ddl `shouldSatisfy` contains (Pattern "name TEXT NOT NULL")
        ddl `shouldSatisfy` contains (Pattern "email TEXT NOT NULL UNIQUE")
        ddl `shouldSatisfy` contains (Pattern "age INTEGER,")

    describe "Extended types DDL" do
      it "generates DDL with JSONB, TIMESTAMPTZ, BIGINT, arrays" do
        eventsDDL `shouldSatisfy` contains (Pattern "CREATE TABLE events")
        eventsDDL `shouldSatisfy` contains (Pattern "metadata JSONB NOT NULL")
        eventsDDL `shouldSatisfy` contains (Pattern "tags TEXT[] NOT NULL")
        eventsDDL `shouldSatisfy` contains (Pattern "created_at TIMESTAMPTZ NOT NULL")
        eventsDDL `shouldSatisfy` contains (Pattern "view_count BIGINT NOT NULL")

    describe "INSERT SQL" do
      it "generates INSERT skipping AutoIncrement columns" do
        insertSQL `shouldSatisfy` contains (Pattern "INSERT INTO users")
        insertSQL `shouldSatisfy` contains (Pattern "(age, email, name)")
        insertSQL `shouldSatisfy` contains (Pattern "VALUES ($1, $2, $3)")
        insertSQL `shouldSatisfy` contains (Pattern "RETURNING *")

    describe "SELECT SQL" do
      it "generates SELECT ALL" do
        selectAllSQL `shouldEqual` "SELECT * FROM users"
      it "generates SELECT WHERE" do
        selectWhereSQL `shouldEqual` "SELECT * FROM users WHERE id = $1"

    describe "Defaults" do
      it "renders DEFAULT in DDL" do
        configDDL `shouldSatisfy` contains (Pattern "active BOOLEAN NOT NULL DEFAULT true")
        configDDL `shouldSatisfy` contains (Pattern "role TEXT NOT NULL DEFAULT 'user'")
        configDDL `shouldSatisfy` contains (Pattern "score INTEGER NOT NULL DEFAULT 0")
      it "skips Default columns in INSERT" do
        configInsertSQL `shouldEqual` "INSERT INTO config () VALUES () RETURNING *"

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
        builderInsert `shouldSatisfy` contains (Pattern "INSERT INTO users")
        builderInsert `shouldSatisfy` contains (Pattern "(age, email, name)")
        builderInsert `shouldSatisfy` contains (Pattern "VALUES ($1, $2, $3)")
      it "builds INSERT with RETURNING" do
        builderInsertReturning `shouldSatisfy` contains (Pattern "INSERT INTO users")
        builderInsertReturning `shouldSatisfy` contains (Pattern "RETURNING id, name")

    describe "Builder UPDATE" do
      it "builds UPDATE SET" do
        builderSet `shouldEqual` "UPDATE users SET name = $1"
      it "builds UPDATE SET with WHERE" do
        builderSetWhere `shouldEqual` "UPDATE users SET name = $1 WHERE id = $id"

    describe "Builder ON CONFLICT" do
      it "builds INSERT ON CONFLICT DO NOTHING" do
        builderUpsert `shouldSatisfy` contains (Pattern "ON CONFLICT (email) DO NOTHING")

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

integrationSpec :: PG.Connection -> Spec Unit
integrationSpec conn = do
  describe "Builder query execution" do
    it "selectAll returns all rows" do
      rows <- from usersTable # selectAll # runQuery conn {}
      Array.length rows `shouldEqual` 2
      let names = map _.name rows
      names `shouldSatisfy` Array.elem "Alice"
      names `shouldSatisfy` Array.elem "Bob"

    it "select with columns returns projected rows" do
      rows <- from usersTable
        # select @"name, email"
        # where_ @"age > $age"
        # runQuery conn { age: 25 }
      Array.length rows `shouldEqual` 1
      (map _.name rows) `shouldEqual` [ "Bob" ]

    it "select with alias" do
      rows <- from usersTable
        # select @"name, email AS e"
        # runQuery conn {}
      Array.length rows `shouldEqual` 2
      (map _.e rows) `shouldSatisfy` Array.elem "alice@example.com"

    it "runQueryOne returns Just for match" do
      result <- from usersTable
        # select @"name, email"
        # where_ @"name = $name"
        # runQueryOne conn { name: "Alice" }
      case result of
        Just r -> r.email `shouldEqual` "alice@example.com"
        Nothing -> shouldEqual "found" "nothing"

    it "runQueryOne returns Nothing for no match" do
      result <- from usersTable
        # selectAll
        # where_ @"name = $name"
        # runQueryOne conn { name: "Nobody" }
      result `shouldEqual` Nothing

    it "complex where with multiple params" do
      rows <- from usersTable
        # selectAll
        # where_ @"name = $name AND age > $age"
        # runQuery conn { name: "Bob", age: 20 }
      Array.length rows `shouldEqual` 1
      (map _.age rows) `shouldEqual` [ Just 30 ]

  describe "Builder insert execution" do
    it "inserts a row and returns count" do
      count <- from usersTable
        # insert { name: "Charlie", email: "charlie@example.com", age: Just 28 }
        # runExecute conn {}
      count `shouldEqual` 1
      rows <- from usersTable
        # select @"name"
        # where_ @"name = $name"
        # runQuery conn { name: "Charlie" }
      Array.length rows `shouldEqual` 1

    it "inserts with RETURNING" do
      rows <- from usersTable
        # insert { name: "Diana", email: "diana@example.com" }
        # returning @"id, name, email"
        # runQuery conn {}
      Array.length rows `shouldEqual` 1
      (map _.name rows) `shouldEqual` [ "Diana" ]
      (map _.email rows) `shouldEqual` [ "diana@example.com" ]

  describe "Builder update execution" do
    it "updates a row" do
      count <- from usersTable
        # set { email: "alice-updated@example.com" }
        # where_ @"name = $name"
        # runExecute conn { name: "Alice" }
      count `shouldEqual` 1
      result <- from usersTable
        # select @"email"
        # where_ @"name = $name"
        # runQueryOne conn { name: "Alice" }
      case result of
        Just r -> r.email `shouldEqual` "alice-updated@example.com"
        Nothing -> shouldEqual "found" "nothing"

    it "updates with RETURNING" do
      rows <- from usersTable
        # set { age: Just 99 }
        # where_ @"name = $name"
        # returning @"name, age"
        # runQuery conn { name: "Bob" }
      Array.length rows `shouldEqual` 1
      (map _.age rows) `shouldEqual` [ Just 99 ]

  describe "Builder upsert execution" do
    it "ON CONFLICT DO NOTHING skips duplicate" do
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
      Array.length rows `shouldEqual` 1
      (map _.name rows) `shouldEqual` [ "Updated" ]
      (map _.age rows) `shouldEqual` [ Just 40 ]

  describe "Builder order by / limit / offset execution" do
    it "ORDER BY sorts results" do
      rows <- from usersTable
        # select @"name"
        # orderBy @"name ASC"
        # runQuery conn {}
      let names = map _.name rows
      names `shouldEqual` (Array.sort names)

    it "LIMIT restricts row count" do
      rows <- from usersTable
        # selectAll
        # orderBy @"name"
        # limit 1
        # runQuery conn {}
      Array.length rows `shouldEqual` 1

    it "OFFSET skips rows" do
      rows <- from usersTable
        # select @"name"
        # orderBy @"name ASC"
        # limit 1
        # offset 1
        # runQuery conn {}
      Array.length rows `shouldEqual` 1
      allRows <- from usersTable
        # select @"name"
        # orderBy @"name ASC"
        # runQuery conn {}
      let allNames = map _.name allRows
      (map _.name rows) `shouldEqual` (Array.take 1 (Array.drop 1 allNames))

  describe "Builder delete execution" do
    it "deletes with RETURNING" do
      _ <- from usersTable
        # insert { name: "ToDelete", email: "delete@example.com", age: Just 50 }
        # runExecute conn {}
      rows <- from usersTable
        # delete
        # where_ @"name = $name"
        # returning @"name, email"
        # runQuery conn { name: "ToDelete" }
      Array.length rows `shouldEqual` 1
      (map _.name rows) `shouldEqual` [ "ToDelete" ]
      -- Verify actually deleted
      result <- from usersTable
        # selectAll
        # where_ @"name = $name"
        # runQueryOne conn { name: "ToDelete" }
      result `shouldEqual` Nothing
