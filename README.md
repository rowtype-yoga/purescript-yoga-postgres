# purescript-yoga-postgres

**Note**: This is a fork of [node-postgres](https://github.com/epost/purescript-node-postgres) ([MIT Licence](./LICENSE/purescript-node-postgress.LICENSE)).


PureScript bindings for the [[https://www.npmjs.org/package/pg][pg library]] ([[https://github.com/brianc/node-postgres][node-postgres]] on GitHub).

## Installation

Clone the project and install its dependencies:

```bash
npm install pg --save
spago install yoga-postgres
```

## Building

Build:

```
npm run build
```
## Testing

Assuming you have [[http://www.postgresql.org/][PostgreSQL]] installed, create a database with some test data:

```
# start a postgres database
docker-compose up
```
Then run the tests:

```
npm run test
```
