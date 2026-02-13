module Test.Postgres.Integration where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Test.Postgres.Schema as Schema
import Test.Spec.Reporter.Console (consoleReporter)
import Yoga.Postgres.Schema (createTableDDL)
import Test.Spec.Runner (runSpec)
import Yoga.Postgres as PG

main :: Effect Unit
main = launchAff_ do
  conn <- liftEffect $ PG.postgres
    { host: PG.PostgresHost "localhost"
    , port: PG.PostgresPort 5433
    , database: PG.PostgresDatabase "test_playground"
    , username: PG.PostgresUsername "postgres"
    , password: PG.PostgresPassword "postgres"
    }
  _ <- PG.executeSimple (PG.SQL "DROP TABLE IF EXISTS posts") conn
  _ <- PG.executeSimple (PG.SQL "DROP TABLE IF EXISTS users") conn
  _ <- PG.executeSimple (PG.SQL "DROP TABLE IF EXISTS events") conn
  _ <- PG.executeSimple (PG.SQL (createTableDDL @Schema.UsersTable)) conn
  _ <- PG.executeSimple (PG.SQL (createTableDDL @Schema.EventsTable)) conn
  _ <- PG.executeSimple (PG.SQL (createTableDDL @Schema.PostsTable)) conn
  _ <- PG.execute (PG.SQL "INSERT INTO users (name, email, age) VALUES ($1, $2, $3)")
    [ PG.toPGValue "Alice", PG.toPGValue "alice@example.com", PG.toPGValue 22 ]
    conn
  _ <- PG.execute (PG.SQL "INSERT INTO users (name, email, age) VALUES ($1, $2, $3)")
    [ PG.toPGValue "Bob", PG.toPGValue "bob@example.com", PG.toPGValue 30 ]
    conn
  _ <- PG.execute (PG.SQL "INSERT INTO posts (title, body, user_id) VALUES ($1, $2, $3)")
    [ PG.toPGValue "Alice's Post", PG.toPGValue "Hello from Alice", PG.toPGValue 1 ]
    conn
  _ <- PG.execute (PG.SQL "INSERT INTO posts (title, body, user_id) VALUES ($1, $2, $3)")
    [ PG.toPGValue "Bob's Post", PG.toPGValue "Hello from Bob", PG.toPGValue 2 ]
    conn
  runSpec [ consoleReporter ] (Schema.integrationSpec conn)
  PG.end conn
