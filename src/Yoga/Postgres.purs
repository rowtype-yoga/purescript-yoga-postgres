module Yoga.Postgres where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Time.Duration (Milliseconds)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)
import Foreign (Foreign)
import Prim.Row (class Union)
import Promise (Promise)
import Promise.Aff (fromAff, toAffE) as Promise
import Unsafe.Coerce (unsafeCoerce)

-- Opaque Postgres types
foreign import data Connection :: Type
foreign import data Transaction :: Type
foreign import data PGValue :: Type

-- Newtypes for type safety

-- Connection configuration
newtype PostgresHost = PostgresHost String

derive instance Newtype PostgresHost _
derive newtype instance Eq PostgresHost
derive newtype instance Show PostgresHost

newtype PostgresPort = PostgresPort Int

derive instance Newtype PostgresPort _
derive newtype instance Eq PostgresPort
derive newtype instance Show PostgresPort

newtype PostgresDatabase = PostgresDatabase String

derive instance Newtype PostgresDatabase _
derive newtype instance Eq PostgresDatabase
derive newtype instance Show PostgresDatabase

newtype PostgresUsername = PostgresUsername String

derive instance Newtype PostgresUsername _
derive newtype instance Eq PostgresUsername
derive newtype instance Show PostgresUsername

newtype PostgresPassword = PostgresPassword String

derive instance Newtype PostgresPassword _
derive newtype instance Eq PostgresPassword
derive newtype instance Show PostgresPassword

newtype ConnectionString = ConnectionString String

derive instance Newtype ConnectionString _
derive newtype instance Eq ConnectionString
derive newtype instance Show ConnectionString

newtype MaxConnections = MaxConnections Int

derive instance Newtype MaxConnections _
derive newtype instance Eq MaxConnections
derive newtype instance Ord MaxConnections
derive newtype instance Show MaxConnections

newtype IdleTimeout = IdleTimeout Milliseconds

derive instance Newtype IdleTimeout _
derive newtype instance Eq IdleTimeout
derive newtype instance Ord IdleTimeout
derive newtype instance Show IdleTimeout

newtype ConnectTimeout = ConnectTimeout Milliseconds

derive instance Newtype ConnectTimeout _
derive newtype instance Eq ConnectTimeout
derive newtype instance Ord ConnectTimeout
derive newtype instance Show ConnectTimeout

newtype StatementTimeout = StatementTimeout Milliseconds

derive instance Newtype StatementTimeout _
derive newtype instance Eq StatementTimeout
derive newtype instance Ord StatementTimeout
derive newtype instance Show StatementTimeout

-- Query types
newtype SQL = SQL String

derive instance Newtype SQL _
derive newtype instance Eq SQL
derive newtype instance Show SQL

-- Type-safe SQL parameters
class ToPGValue a where
  toPGValue :: a -> PGValue

instance ToPGValue String where
  toPGValue = unsafeCoerce

instance ToPGValue Int where
  toPGValue = unsafeCoerce

instance ToPGValue Number where
  toPGValue = unsafeCoerce

instance ToPGValue Boolean where
  toPGValue = unsafeCoerce

instance ToPGValue a => ToPGValue (Array a) where
  toPGValue arr = unsafeCoerce (map toPGValue arr)

instance ToPGValue Foreign where
  toPGValue = unsafeCoerce

-- Type-safe parameter builder (just use Array - already has Semigroup/Monoid)
param :: forall a. ToPGValue a => a -> Array PGValue
param x = [ toPGValue x ]

params :: forall a. ToPGValue a => Array a -> Array PGValue
params xs = map toPGValue xs

-- Result types
type Row = Foreign

type QueryResult =
  { rows :: Array Row
  , count :: Int
  , command :: String
  }

-- Create Postgres connection
type PostgresConfigImpl =
  ( host :: PostgresHost
  , port :: PostgresPort
  , database :: PostgresDatabase
  , username :: PostgresUsername
  , password :: PostgresPassword
  , connection :: ConnectionString
  , max :: MaxConnections
  , idle_timeout :: IdleTimeout
  , connect_timeout :: ConnectTimeout
  , ssl :: Boolean
  , debug :: Boolean
  , onnotice :: Effect Unit
  , onparameter :: Foreign -> Effect Unit
  )

foreign import postgresImpl :: forall opts. EffectFn1 { | opts } Connection

postgres :: forall opts opts_. Union opts opts_ PostgresConfigImpl => { | opts } -> Effect Connection
postgres opts = runEffectFn1 postgresImpl opts

-- Query operations

-- Execute query with parameters
foreign import queryImpl :: EffectFn3 Connection SQL (Array PGValue) (Promise QueryResult)

query :: SQL -> Array PGValue -> Connection -> Aff QueryResult
query sql params conn = runEffectFn3 queryImpl conn sql params # Promise.toAffE

-- Execute simple query (no parameters)
foreign import querySimpleImpl :: EffectFn2 Connection SQL (Promise QueryResult)

querySimple :: SQL -> Connection -> Aff QueryResult
querySimple sql conn = runEffectFn2 querySimpleImpl conn sql # Promise.toAffE

-- Query one row with parameters (returns Maybe)
foreign import queryOneImpl :: EffectFn3 Connection SQL (Array PGValue) (Promise (Nullable Row))

queryOne :: SQL -> Array PGValue -> Connection -> Aff (Maybe Row)
queryOne sql params conn = runEffectFn3 queryOneImpl conn sql params # Promise.toAffE <#> Nullable.toMaybe

-- Query one row simple (no parameters, returns Maybe)
foreign import queryOneSimpleImpl :: EffectFn2 Connection SQL (Promise (Nullable Row))

queryOneSimple :: SQL -> Connection -> Aff (Maybe Row)
queryOneSimple sql conn = runEffectFn2 queryOneSimpleImpl conn sql # Promise.toAffE <#> Nullable.toMaybe

-- Unsafe operations that return exact row or throw
foreign import unsafeImpl :: EffectFn3 Connection SQL (Array PGValue) (Promise Row)

unsafe :: SQL -> Array PGValue -> Connection -> Aff Row
unsafe sql params conn = runEffectFn3 unsafeImpl conn sql params # Promise.toAffE

-- Execute query (for INSERT/UPDATE/DELETE without caring about results)
foreign import executeImpl :: EffectFn3 Connection SQL (Array PGValue) (Promise Int)

execute :: SQL -> Array PGValue -> Connection -> Aff Int
execute sql params conn = runEffectFn3 executeImpl conn sql params # Promise.toAffE

-- Execute simple (no parameters)
foreign import executeSimpleImpl :: EffectFn2 Connection SQL (Promise Int)

executeSimple :: SQL -> Connection -> Aff Int
executeSimple sql conn = runEffectFn2 executeSimpleImpl conn sql # Promise.toAffE

-- Transaction operations

-- Begin transaction
foreign import beginImpl :: EffectFn1 Connection (Promise Transaction)

begin :: Connection -> Aff Transaction
begin = runEffectFn1 beginImpl >>> Promise.toAffE

-- Commit transaction
foreign import commitImpl :: EffectFn1 Transaction (Promise Unit)

commit :: Transaction -> Aff Unit
commit = runEffectFn1 commitImpl >>> Promise.toAffE

-- Rollback transaction
foreign import rollbackImpl :: EffectFn1 Transaction (Promise Unit)

rollback :: Transaction -> Aff Unit
rollback = runEffectFn1 rollbackImpl >>> Promise.toAffE

-- Run transaction block (automatic commit/rollback)
foreign import transactionImpl :: EffectFn2 Connection (Transaction -> Effect (Promise Unit)) (Promise Unit)

transaction :: (Transaction -> Aff Unit) -> Connection -> Aff Unit
transaction handler conn =
  runEffectFn2 transactionImpl conn (\txn -> Promise.fromAff (handler txn))
    # Promise.toAffE

-- Query within transaction
foreign import txQueryImpl :: EffectFn3 Transaction SQL (Array PGValue) (Promise QueryResult)

txQuery :: SQL -> Array PGValue -> Transaction -> Aff QueryResult
txQuery sql params txn = runEffectFn3 txQueryImpl txn sql params # Promise.toAffE

foreign import txQuerySimpleImpl :: EffectFn2 Transaction SQL (Promise QueryResult)

txQuerySimple :: SQL -> Transaction -> Aff QueryResult
txQuerySimple sql txn = runEffectFn2 txQuerySimpleImpl txn sql # Promise.toAffE

foreign import txExecuteImpl :: EffectFn3 Transaction SQL (Array PGValue) (Promise Int)

txExecute :: SQL -> Array PGValue -> Transaction -> Aff Int
txExecute sql params txn = runEffectFn3 txExecuteImpl txn sql params # Promise.toAffE

-- Connection management

-- End connection/pool
foreign import endImpl :: EffectFn1 Connection (Promise Unit)

end :: Connection -> Aff Unit
end = runEffectFn1 endImpl >>> Promise.toAffE

-- Listen for notifications (LISTEN/NOTIFY)
newtype Channel = Channel String

derive instance Newtype Channel _
derive newtype instance Eq Channel
derive newtype instance Show Channel

type Notification =
  { channel :: Channel
  , payload :: String
  }

foreign import listenImpl :: EffectFn3 Connection Channel (Notification -> Effect Unit) (Promise Unit)

listen :: Channel -> (Notification -> Effect Unit) -> Connection -> Aff Unit
listen channel handler conn = runEffectFn3 listenImpl conn channel handler # Promise.toAffE

foreign import unlistenImpl :: EffectFn2 Connection Channel (Promise Unit)

unlisten :: Channel -> Connection -> Aff Unit
unlisten channel conn = runEffectFn2 unlistenImpl conn channel # Promise.toAffE

foreign import notifyImpl :: EffectFn3 Connection Channel String (Promise Unit)

notify :: Channel -> String -> Connection -> Aff Unit
notify channel payload conn = runEffectFn3 notifyImpl conn channel payload # Promise.toAffE

-- Prepared statements
newtype StatementName = StatementName String

derive instance Newtype StatementName _
derive newtype instance Eq StatementName
derive newtype instance Show StatementName

type PreparedStatement =
  { name :: StatementName
  , query :: SQL
  }

foreign import prepareImpl :: EffectFn3 Connection StatementName SQL (Promise Unit)

prepare :: StatementName -> SQL -> Connection -> Aff Unit
prepare name sql conn = runEffectFn3 prepareImpl conn name sql # Promise.toAffE

foreign import executePreparedImpl :: EffectFn3 Connection StatementName (Array PGValue) (Promise QueryResult)

executePrepared :: StatementName -> Array PGValue -> Connection -> Aff QueryResult
executePrepared name ps conn = runEffectFn3 executePreparedImpl conn name ps # Promise.toAffE

foreign import deallocateImpl :: EffectFn2 Connection StatementName (Promise Unit)

deallocate :: StatementName -> Connection -> Aff Unit
deallocate name conn = runEffectFn2 deallocateImpl conn name # Promise.toAffE

-- Utility functions

-- Check connection health
foreign import pingImpl :: EffectFn1 Connection (Promise Boolean)

ping :: Connection -> Aff Boolean
ping = runEffectFn1 pingImpl >>> Promise.toAffE

-- Get connection options
foreign import optionsImpl :: EffectFn1 Connection { host :: String, port :: Int, database :: String }

options :: Connection -> Effect { host :: String, port :: Int, database :: String }
options = runEffectFn1 optionsImpl
