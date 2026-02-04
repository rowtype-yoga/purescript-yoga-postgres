import postgres from 'postgres';

// Create Postgres connection
export const postgresImpl = (config) => {
  // Support both individual config and connection string
  if (config.connection) {
    return postgres(config.connection, config);
  }
  
  return postgres({
    host: config.host,
    port: config.port,
    database: config.database,
    username: config.username,
    password: config.password,
    max: config.max,
    idle_timeout: config.idle_timeout,
    connect_timeout: config.connect_timeout,
    ssl: config.ssl,
    debug: config.debug,
    onnotice: config.onnotice,
    onparameter: config.onparameter,
    // Pass through any other options
    ...config
  });
};

// Query operations

export const queryImpl = async (sql, queryString, params) => {
  const result = await sql.unsafe(queryString, params);
  return {
    rows: result,
    count: result.count,
    command: result.command
  };
};

export const querySimpleImpl = async (sql, queryString) => {
  const result = await sql.unsafe(queryString);
  return {
    rows: result,
    count: result.count,
    command: result.command
  };
};

export const queryOneImpl = async (sql, queryString, params) => {
  const result = await sql.unsafe(queryString, params);
  return result.length > 0 ? result[0] : null;
};

export const queryOneSimpleImpl = async (sql, queryString) => {
  const result = await sql.unsafe(queryString);
  return result.length > 0 ? result[0] : null;
};

export const unsafeImpl = async (sql, queryString, params) => {
  const result = await sql.unsafe(queryString, params);
  if (result.length === 0) {
    throw new Error('Expected exactly one row, got zero');
  }
  return result[0];
};

export const executeImpl = async (sql, queryString, params) => {
  const result = await sql.unsafe(queryString, params);
  return result.count ?? 0;
};

export const executeSimpleImpl = async (sql, queryString) => {
  const result = await sql.unsafe(queryString);
  return result.count ?? 0;
};

// Transaction operations

// For manual transaction control, we need to reserve a connection
// postgres.js requires this to avoid "UNSAFE_TRANSACTION" errors
export const beginImpl = async (sql) => {
  // Reserve a connection from the pool
  const reserved = await sql.reserve();
  
  // Begin transaction on the reserved connection
  await reserved`BEGIN`;
  
  // Return an object that tracks the reserved connection
  return {
    _reserved: reserved,
    _sql: reserved
  };
};

export const commitImpl = async (txn) => {
  if (txn._reserved) {
    await txn._reserved`COMMIT`;
    // Release the connection back to the pool
    await txn._reserved.release();
  }
};

export const rollbackImpl = async (txn) => {
  if (txn._reserved) {
    await txn._reserved`ROLLBACK`;
    // Release the connection back to the pool
    await txn._reserved.release();
  }
};

export const transactionImpl = async (sql, handler) => {
  return sql.begin(async (txn) => {
    const affAction = handler(txn);
    return await affAction();
  });
};

export const txQueryImpl = async (txn, queryString, params) => {
  // Use the underlying SQL connection
  const sql = txn._sql || txn;
  const result = await sql.unsafe(queryString, params);
  return {
    rows: result,
    count: result.count,
    command: result.command
  };
};

export const txQuerySimpleImpl = async (txn, queryString) => {
  const sql = txn._sql || txn;
  const result = await sql.unsafe(queryString);
  return {
    rows: result,
    count: result.count,
    command: result.command
  };
};

export const txExecuteImpl = async (txn, queryString, params) => {
  const sql = txn._sql || txn;
  const result = await sql.unsafe(queryString, params);
  return result.count ?? 0;
};

// Connection management

export const endImpl = (sql) => sql.end();

// Listen/Notify

export const listenImpl = async (sql, channel, handler) => {
  await sql.listen(channel, (payload) => {
    handler({ channel, payload })();
  });
};

export const unlistenImpl = async (sql, channel) => {
  // postgres.js uses the same listen method to unlisten
  if (sql.listen.unlisten) {
    return sql.listen.unlisten(channel);
  }
  // Alternative: just resolve, as postgres.js handles cleanup automatically
  return Promise.resolve();
};

export const notifyImpl = (sql, channel, payload) => 
  sql.notify(channel, payload);

// Prepared statements

// Store prepared statements in a WeakMap
const preparedStatements = new WeakMap();

export const prepareImpl = async (sql, name, queryString) => {
  // Store the query string associated with this statement name
  if (!preparedStatements.has(sql)) {
    preparedStatements.set(sql, new Map());
  }
  preparedStatements.get(sql).set(name, queryString);
};

export const executePreparedImpl = async (sql, name, params) => {
  // Retrieve the query string for this prepared statement
  const stmts = preparedStatements.get(sql);
  if (!stmts || !stmts.has(name)) {
    throw new Error(`Prepared statement "${name}" not found`);
  }
  const queryString = stmts.get(name);
  
  // Execute using the stored query string
  const result = await sql.unsafe(queryString, params);
  return {
    rows: result,
    count: result.count ?? 0,
    command: result.command
  };
};

export const deallocateImpl = async (sql, name) => {
  // Remove the prepared statement from our map
  const stmts = preparedStatements.get(sql);
  if (stmts) {
    stmts.delete(name);
  }
};

// Utility functions

export const pingImpl = async (sql) => {
  try {
    await sql`SELECT 1`;
    return true;
  } catch (err) {
    return false;
  }
};

export const optionsImpl = (sql) => {
  const opts = sql.options;
  return {
    host: String(opts.host || opts.hostname || 'localhost'),
    port: Number(opts.port || 5432),
    database: String(opts.database || opts.db || '')
  };
};
