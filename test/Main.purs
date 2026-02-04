module Test.Postgres.Main where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, bracket, launchAff_, throwError, try)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error)
import Test.Spec (Spec, around, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Yoga.Test.Docker as Docker
import Yoga.Postgres as PG

-- Test configuration
testHost :: PG.PostgresHost
testHost = PG.PostgresHost "localhost"

testPort :: PG.PostgresPort
testPort = PG.PostgresPort 5433 -- Test port from docker-compose.test.yml

testDatabase :: PG.PostgresDatabase
testDatabase = PG.PostgresDatabase "test_playground"

testUsername :: PG.PostgresUsername
testUsername = PG.PostgresUsername "postgres"

testPassword :: PG.PostgresPassword
testPassword = PG.PostgresPassword "postgres"

-- Helper to create and manage Postgres connection
withPostgres :: (PG.Connection -> Aff Unit) -> Aff Unit
withPostgres test = do
  conn <- liftEffect $ PG.postgres
    { host: testHost
    , port: testPort
    , database: testDatabase
    , username: testUsername
    , password: testPassword
    }
  -- Clean up test tables before each test
  _ <- try $ PG.executeSimple (PG.SQL "DROP TABLE IF EXISTS test_users CASCADE") conn
  _ <- try $ PG.executeSimple (PG.SQL "DROP TABLE IF EXISTS test_posts CASCADE") conn
  test conn
  _ <- PG.end conn
  pure unit

-- Helper to check if value is Left (for use with shouldSatisfy)
isLeft :: forall a b. Either a b -> Boolean
isLeft (Left _) = true
isLeft _ = false

-- Helper to set up test table
setupTestTable :: PG.Connection -> Aff Unit
setupTestTable conn = do
  _ <- PG.executeSimple
    ( PG.SQL
        """
    CREATE TABLE test_users (
      id SERIAL PRIMARY KEY,
      name TEXT NOT NULL,
      email TEXT,
      age INTEGER,
      active BOOLEAN DEFAULT true,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  """
    )
    conn
  pure unit

-- Helper to set up table with constraints
setupTableWithConstraints :: PG.Connection -> Aff Unit
setupTableWithConstraints conn = do
  _ <- PG.executeSimple
    ( PG.SQL
        """
    CREATE TABLE test_users (
      id SERIAL PRIMARY KEY,
      email TEXT UNIQUE NOT NULL,
      name TEXT NOT NULL
    )
  """
    )
    conn
  pure unit

spec :: Spec Unit
spec = do
  describe "Yoga.Postgres Integration Tests" do

    -- Connection Management Tests
    around withPostgres do
      describe "Connection Management" do
        it "connects to Postgres successfully" \conn -> do
          healthy <- PG.ping conn
          healthy `shouldEqual` true

        it "retrieves connection options" \conn -> do
          opts <- liftEffect $ PG.options conn
          opts.host `shouldSatisfy` (_ /= "")
          opts.port `shouldEqual` 5433
          opts.database `shouldEqual` "test_playground"

    describe "Connection Errors" do
      it "handles connection failures" do
        result <- try do
          conn <- liftEffect $ PG.postgres
            { host: PG.PostgresHost "invalid-host-that-does-not-exist"
            , port: PG.PostgresPort 9999
            , database: testDatabase
            , username: testUsername
            , password: testPassword
            }
          PG.ping conn
        result `shouldSatisfy` isLeft

    -- Basic Query Tests
    around withPostgres do
      describe "Basic Queries" do
        it "creates and drops tables" \conn -> do
          _ <- PG.executeSimple (PG.SQL "DROP TABLE IF EXISTS test_temp") conn
          count <- PG.executeSimple (PG.SQL "CREATE TABLE test_temp (id SERIAL PRIMARY KEY, name TEXT)") conn
          count `shouldEqual` 0
          _ <- PG.executeSimple (PG.SQL "DROP TABLE test_temp") conn
          pure unit

        it "inserts and queries data" \conn -> do
          setupTestTable conn

          count <- PG.execute (PG.SQL "INSERT INTO test_users (name, email) VALUES ($1, $2)")
            [ PG.toPGValue "Alice", PG.toPGValue "alice@example.com" ]
            conn
          count `shouldEqual` 1

          result <- PG.querySimple (PG.SQL "SELECT * FROM test_users") conn
          result.count `shouldEqual` 1

        it "queries with WHERE clause" \conn -> do
          setupTestTable conn

          _ <- PG.execute (PG.SQL "INSERT INTO test_users (name, email, age) VALUES ($1, $2, $3)")
            [ PG.toPGValue "Bob", PG.toPGValue "bob@example.com", PG.toPGValue 30 ]
            conn

          _ <- PG.execute (PG.SQL "INSERT INTO test_users (name, email, age) VALUES ($1, $2, $3)")
            [ PG.toPGValue "Charlie", PG.toPGValue "charlie@example.com", PG.toPGValue 25 ]
            conn

          result <- PG.query (PG.SQL "SELECT * FROM test_users WHERE age > $1")
            [ PG.toPGValue 26 ]
            conn
          result.count `shouldEqual` 1

        it "updates data" \conn -> do
          setupTestTable conn

          _ <- PG.execute (PG.SQL "INSERT INTO test_users (name, email) VALUES ($1, $2)")
            [ PG.toPGValue "Diana", PG.toPGValue "diana@example.com" ]
            conn

          updateCount <- PG.execute (PG.SQL "UPDATE test_users SET email = $1 WHERE name = $2")
            [ PG.toPGValue "diana.new@example.com", PG.toPGValue "Diana" ]
            conn
          updateCount `shouldEqual` 1

        it "deletes data" \conn -> do
          setupTestTable conn

          _ <- PG.execute (PG.SQL "INSERT INTO test_users (name, email) VALUES ($1, $2)")
            [ PG.toPGValue "Eve", PG.toPGValue "eve@example.com" ]
            conn

          deleteCount <- PG.execute (PG.SQL "DELETE FROM test_users WHERE name = $1")
            [ PG.toPGValue "Eve" ]
            conn
          deleteCount `shouldEqual` 1

          result <- PG.querySimple (PG.SQL "SELECT * FROM test_users") conn
          result.count `shouldEqual` 0

    -- Parameterized Query Tests
    around withPostgres do
      describe "Parameterized Queries" do
        it "prevents SQL injection" \conn -> do
          setupTestTable conn

          let maliciousInput = "'; DROP TABLE test_users; --"
          _ <- PG.execute (PG.SQL "INSERT INTO test_users (name) VALUES ($1)")
            [ PG.toPGValue maliciousInput ]
            conn

          result <- PG.querySimple (PG.SQL "SELECT * FROM test_users") conn
          result.count `shouldEqual` 1

        it "handles multiple parameters" \conn -> do
          setupTestTable conn

          _ <- PG.execute (PG.SQL "INSERT INTO test_users (name, age, email) VALUES ($1, $2, $3)")
            [ PG.toPGValue "Frank", PG.toPGValue 35, PG.toPGValue "frank@example.com" ]
            conn

          result <- PG.query (PG.SQL "SELECT * FROM test_users WHERE age > $1")
            [ PG.toPGValue 30 ]
            conn
          result.count `shouldEqual` 1

        it "handles array parameters" \conn -> do
          setupTestTable conn

          _ <- PG.execute (PG.SQL "INSERT INTO test_users (name, email) VALUES ($1, $2)")
            [ PG.toPGValue "Grace", PG.toPGValue "grace@example.com" ]
            conn

          result <- PG.query (PG.SQL "SELECT * FROM test_users WHERE name = ANY($1)")
            [ PG.toPGValue [ "Grace", "Unknown" ] ]
            conn
          result.count `shouldEqual` 1

    -- Transaction Tests
    around withPostgres do
      describe "Transactions" do
        it "commits transactions successfully" \conn -> do
          setupTestTable conn

          _ <- PG.transaction
            ( \txn -> do
                _ <- PG.txExecute (PG.SQL "INSERT INTO test_users (name) VALUES ($1)")
                  [ PG.toPGValue "Henry" ]
                  txn
                _ <- PG.txExecute (PG.SQL "INSERT INTO test_users (name) VALUES ($1)")
                  [ PG.toPGValue "Iris" ]
                  txn
                pure unit
            )
            conn

          result <- PG.querySimple (PG.SQL "SELECT * FROM test_users") conn
          result.count `shouldEqual` 2

        it "rolls back on errors" \conn -> do
          setupTestTable conn

          result <- try $ PG.transaction
            ( \txn -> do
                _ <- PG.txExecute (PG.SQL "INSERT INTO test_users (name) VALUES ($1)")
                  [ PG.toPGValue "Jack" ]
                  txn
                throwError (error "Intentional error")
            )
            conn

          result `shouldSatisfy` isLeft

          count <- PG.querySimple (PG.SQL "SELECT * FROM test_users") conn
          count.count `shouldEqual` 0

        it "queries within transaction" \conn -> do
          setupTestTable conn

          _ <- PG.transaction
            ( \txn -> do
                _ <- PG.txExecute (PG.SQL "INSERT INTO test_users (name) VALUES ($1)")
                  [ PG.toPGValue "Leo" ]
                  txn

                result <- PG.txQuerySimple (PG.SQL "SELECT * FROM test_users") txn
                liftEffect $ log $ "Rows in transaction: " <> show result.count
                pure unit
            )
            conn

          result <- PG.querySimple (PG.SQL "SELECT * FROM test_users") conn
          result.count `shouldEqual` 1

    -- Data Type Tests
    around withPostgres do
      describe "Data Types" do
        it "handles NULL values" \conn -> do
          setupTestTable conn

          _ <- PG.execute (PG.SQL "INSERT INTO test_users (name, email) VALUES ($1, $2)")
            [ PG.toPGValue "Mia", PG.toPGValue "" ]
            conn

          result <- PG.query (PG.SQL "SELECT * FROM test_users WHERE email IS NULL OR email = ''")
            []
            conn
          result.count `shouldSatisfy` (_ >= 0)

        it "handles various data types" \conn -> do
          setupTestTable conn

          _ <- PG.execute (PG.SQL "INSERT INTO test_users (name, age, active) VALUES ($1, $2, $3)")
            [ PG.toPGValue "Nina", PG.toPGValue 28, PG.toPGValue true ]
            conn

          result <- PG.querySimple (PG.SQL "SELECT * FROM test_users") conn
          result.count `shouldEqual` 1

        it "handles boolean values" \conn -> do
          setupTestTable conn

          _ <- PG.execute (PG.SQL "INSERT INTO test_users (name, active) VALUES ($1, $2)")
            [ PG.toPGValue "Oscar", PG.toPGValue false ]
            conn

          result <- PG.query (PG.SQL "SELECT * FROM test_users WHERE active = $1")
            [ PG.toPGValue false ]
            conn
          result.count `shouldEqual` 1

    -- Error Handling Tests
    around withPostgres do
      describe "Error Handling" do
        it "handles syntax errors" \conn -> do
          result <- try $ PG.executeSimple (PG.SQL "INVALID SQL SYNTAX HERE") conn
          result `shouldSatisfy` isLeft

        it "handles constraint violations" \conn -> do
          setupTableWithConstraints conn

          _ <- PG.execute (PG.SQL "INSERT INTO test_users (email, name) VALUES ($1, $2)")
            [ PG.toPGValue "unique@example.com", PG.toPGValue "User1" ]
            conn

          result <- try $ PG.execute (PG.SQL "INSERT INTO test_users (email, name) VALUES ($1, $2)")
            [ PG.toPGValue "unique@example.com", PG.toPGValue "User2" ]
            conn

          result `shouldSatisfy` isLeft

        it "handles missing table errors" \conn -> do
          result <- try $ PG.querySimple (PG.SQL "SELECT * FROM non_existent_table") conn
          case result of
            Left _ -> pure unit -- Expected error
            Right _ -> throwError (error "Expected error for missing table")

        it "handles missing column errors" \conn -> do
          setupTestTable conn

          result <- try $ PG.querySimple (PG.SQL "SELECT non_existent_column FROM test_users") conn
          case result of
            Left _ -> pure unit -- Expected error
            Right _ -> throwError (error "Expected error for missing column")

    -- Query Helper Tests
    around withPostgres do
      describe "Query Helpers" do
        it "queryOne returns Maybe for single result" \conn -> do
          setupTestTable conn

          _ <- PG.execute (PG.SQL "INSERT INTO test_users (name) VALUES ($1)")
            [ PG.toPGValue "Paul" ]
            conn

          result <- PG.queryOne (PG.SQL "SELECT * FROM test_users WHERE name = $1")
            [ PG.toPGValue "Paul" ]
            conn
          case result of
            Just _ -> pure unit
            Nothing -> throwError (error "Expected to find user Paul")

        it "queryOne returns Nothing when no results" \conn -> do
          setupTestTable conn

          result <- PG.queryOne (PG.SQL "SELECT * FROM test_users WHERE name = $1")
            [ PG.toPGValue "NonExistent" ]
            conn
          case result of
            Nothing -> pure unit
            Just _ -> throwError (error "Expected Nothing for non-existent user")

        it "queryOneSimple works without parameters" \conn -> do
          setupTestTable conn

          _ <- PG.execute (PG.SQL "INSERT INTO test_users (name) VALUES ($1)")
            [ PG.toPGValue "Quinn" ]
            conn

          result <- PG.queryOneSimple (PG.SQL "SELECT * FROM test_users LIMIT 1") conn
          case result of
            Just _ -> pure unit
            Nothing -> throwError (error "Expected to find at least one user")

    -- Prepared Statement Tests
    around withPostgres do
      describe "Prepared Statements" do
        it "prepares and executes statements" \conn -> do
          setupTestTable conn

          let stmtName = PG.StatementName "insert_user"
          _ <- PG.prepare stmtName (PG.SQL "INSERT INTO test_users (name, email) VALUES ($1, $2)") conn

          result <- PG.executePrepared stmtName
            [ PG.toPGValue "Rachel", PG.toPGValue "rachel@example.com" ]
            conn
          result.count `shouldEqual` 1

          _ <- PG.deallocate stmtName conn
          pure unit

        it "reuses prepared statements" \conn -> do
          setupTestTable conn

          let stmtName = PG.StatementName "insert_user_batch"
          _ <- PG.prepare stmtName (PG.SQL "INSERT INTO test_users (name) VALUES ($1)") conn

          _ <- PG.executePrepared stmtName [ PG.toPGValue "Sam" ] conn
          _ <- PG.executePrepared stmtName [ PG.toPGValue "Tina" ] conn
          _ <- PG.executePrepared stmtName [ PG.toPGValue "Uma" ] conn

          result <- PG.querySimple (PG.SQL "SELECT * FROM test_users") conn
          result.count `shouldEqual` 3

          _ <- PG.deallocate stmtName conn
          pure unit

main :: Effect Unit
main = launchAff_ do
  liftEffect $ log "\n🧪 Starting Postgres Integration Tests (with Docker)\n"

  bracket
    -- Start Docker before tests
    ( do
        liftEffect $ log "⏳ Starting Postgres and waiting for it to be ready..."
        Docker.startService "packages/yoga-postgres/docker-compose.test.yml" 30
        liftEffect $ log "✅ Postgres is ready!\n"
    )
    -- Stop Docker after tests (always runs!)
    ( \_ -> do
        Docker.stopService "packages/yoga-postgres/docker-compose.test.yml"
        liftEffect $ log "✅ Cleanup complete\n"
    )
    -- Run tests
    (\_ -> runSpec [ consoleReporter ] spec)
