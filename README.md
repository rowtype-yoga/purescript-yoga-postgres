# yoga-postgres

Type-safe PureScript FFI bindings for PostgreSQL with typed query support.

## Installation

```bash
spago install yoga-postgres yoga-sql-types
npm install pg
```

## Usage

```purescript
import Yoga.Postgres as PG
import Yoga.Postgres.TypedQuery as TQ

main = launchAff_ do
  conn <- PG.connect { connectionString: "postgresql://localhost/mydb" }
  
  -- Raw queries
  rows <- PG.query conn "SELECT * FROM users" []
  
  -- Typed queries
  result <- TQ.query conn 
    (TQ.sql @"SELECT id, name FROM users WHERE id = $id")
    { id: 123 }
  
  PG.disconnect conn
```

See [yoga-postgres-om](../yoga-postgres-om) for Om-wrapped operations.

## License

MIT
