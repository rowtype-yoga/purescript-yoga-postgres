-- EXPECT: Cons
module Test.CompileFail.OrderByOnUpdate where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\))
import Type.Proxy (Proxy(..))
import Yoga.Postgres.Schema

type UsersTable = Table "users"
  ( id :: Column Int (PrimaryKey /\ AutoIncrement)
  , name :: Column String None
  , email :: Column String Unique
  , age :: Column (Maybe Int) None
  )

usersTable :: Proxy UsersTable
usersTable = Proxy

bad = from usersTable # set { name: "B" } # orderBy @"name"
