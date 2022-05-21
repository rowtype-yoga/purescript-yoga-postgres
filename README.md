# 💾 purescript-yoga-postgres

**Note**: This is a fork of [node-postgres](https://github.com/epost/purescript-node-postgres) ([MIT Licence](./LICENSE/purescript-node-postgress.LICENSE)).


PureScript bindings for the [pg library](https://www.npmjs.org/package/pg) ([node-postgres](https://github.com/brianc/node-postgres) on GitHub).

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

```
# start a postgres database
docker-compose up
```
Then run the tests:

```
npm run test
```
