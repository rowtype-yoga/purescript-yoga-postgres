module Test.Postgres.Schema where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String (contains, Pattern(..))
import Data.Tuple.Nested (type (/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
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
typedSelectAll = from usersTable # selectAll

typedSelectCols
  :: Q "users" _
       (name :: String, email :: String)
       ()
typedSelectCols = from usersTable # select @"name, email"

typedSelectAlias
  :: Q "users" _
       (name :: String, e :: String)
       ()
typedSelectAlias = from usersTable # select @"name, email AS e"

typedWhere
  :: Q "users" _
       (age :: Maybe Int, email :: String, id :: Int, name :: String)
       (id :: Int)
typedWhere = from usersTable # selectAll # where_ @"id = $id"

typedWhereComplex
  :: Q "users" _
       (age :: Maybe Int, email :: String, id :: Int, name :: String)
       (name :: String, age :: Int)
typedWhereComplex = from usersTable # selectAll # where_ @"name = $name AND age > $age"

typedSelectColsWhere
  :: Q "users" _
       (name :: String)
       (age :: Int)
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
-- Phase 9: Query execution (type annotations prove correctness)
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
