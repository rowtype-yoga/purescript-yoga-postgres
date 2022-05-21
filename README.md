# 💾 purescript-yoga-postgres

**Note**: This is a fork of [purescript-node-postgres](https://github.com/epost/purescript-node-postgres) ([MIT Licence](./LICENSE/purescript-node-postgress.LICENSE)).


PureScript bindings for the [pg library](https://www.npmjs.org/package/pg) ([node-postgres](https://github.com/brianc/node-postgres) on GitHub).

## Installation

Clone the project and install its dependencies:

```bash
npm install pg --save
spago install yoga-postgres
```

## Building

Build:

```bash
spago build
# or
npm run build
```

## Testing

```bash
# start a postgres database
docker-compose up
```
Then run the tests:
```bash
spago -x test.dhall test
# or
npm run test
```
