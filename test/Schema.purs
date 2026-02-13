module Test.Postgres.Schema where

import Prelude

import Data.Maybe (Maybe)
import Data.String (contains, Pattern(..))
import Data.Tuple.Nested (type (/\))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
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

-- allUsers :: PG.Connection -> Aff (Either String (Array { id :: Int, name :: String, email :: String, age :: Maybe Int }))
-- allUsers conn = selectAll @UsersTable conn

-- userById :: PG.Connection -> Aff (Either String (Array { id :: Int, name :: String, email :: String, age :: Maybe Int }))
-- userById conn = selectWhere @UsersTable { id: 1 } conn

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Phase 5: Type-level defaults
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- type ConfigTable = Table "config"
--   ( id     :: Column Int PrimaryKey
--   , active :: Column Boolean (Default True)
--   , role   :: Column String (Default "user")
--   , score  :: Column Int (Default 0)
--   )

-- configDDL :: String
-- configDDL = createTableDDL @ConfigTable
-- -- Should contain: DEFAULT true, DEFAULT 'user', DEFAULT 0

-- insertConfig :: PG.Connection -> Aff (Either String { id :: Int, active :: Boolean, role :: String, score :: Int })
-- insertConfig conn = insert @ConfigTable {} conn
-- -- All non-PK columns have defaults, so empty record is fine

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Phase 6: Type-safe UPDATE
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- updateResult :: PG.Connection -> Aff Int
-- updateResult conn = update @UsersTable { name: "Bob" } { id: 1 } conn

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Phase 7: Type-safe DELETE
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- deleteResult :: PG.Connection -> Aff Int
-- deleteResult conn = delete @UsersTable { id: 1 } conn

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
      it "generates INSERT skipping PrimaryKey columns" do
        insertSQL `shouldSatisfy` contains (Pattern "INSERT INTO users")
        insertSQL `shouldSatisfy` contains (Pattern "(age, email, name)")
        insertSQL `shouldSatisfy` contains (Pattern "VALUES ($1, $2, $3)")
        insertSQL `shouldSatisfy` contains (Pattern "RETURNING *")
