module Test.Postgres.Schema where

import Prelude

import Data.Maybe (Maybe)
import Data.String (contains, Pattern(..))
import Data.Tuple.Nested (type (/\))
import Prim.Boolean (True)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Type.Proxy (Proxy(..))
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

builderSelectAll :: String
builderSelectAll = from usersTable # selectAll # toSQL

builderSelectCols :: String
builderSelectCols = from usersTable # select @"name, email" # toSQL

builderSelectWhere :: String
builderSelectWhere = from usersTable # selectAll # where_ @(id :: Int) # toSQL

builderSelectColsWhere :: String
builderSelectColsWhere = from usersTable # select @"name" # where_ @(age :: Int) # toSQL

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
        builderSelectWhere `shouldEqual` "SELECT * FROM users WHERE id = $1"
      it "builds SELECT columns with WHERE" do
        builderSelectColsWhere `shouldEqual` "SELECT name FROM users WHERE age = $1"
