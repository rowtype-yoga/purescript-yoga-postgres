-- EXPECT: Cons
module Test.CompileFail.OnConflictWithoutInsert where

import Prelude
import Data.Tuple.Nested (type (/\))
import Type.Proxy (Proxy(..))
import Yoga.Postgres.Schema

type UsersTable = Table "users"
  ( id :: Column Int (PrimaryKey /\ AutoIncrement)
  , name :: Column String None
  )

usersTable :: Proxy UsersTable
usersTable = Proxy

bad = from usersTable # selectAll # onConflictDoNothing @"name"
