-- EXPECT: Lacks
module Test.CompileFail.WhereOnInsert where

import Data.Maybe (Maybe(..))
import Prelude
import Data.Tuple.Nested (type (/\))
import Type.Proxy (Proxy(..))
import Yoga.Postgres.Schema

type UsersTable = Table "users"
  ( id :: Column Int (PrimaryKey /\ AutoIncrement)
  , name :: Column String None
  , age :: Column (Maybe Int) None
  )

usersTable :: Proxy UsersTable
usersTable = Proxy

bad = from usersTable # insert { name: "Alice", age: Nothing } # where_ @"id = $id"
