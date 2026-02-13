-- EXPECT: Cons
module Test.CompileFail.LimitWithoutSelect where

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

bad = from usersTable # set { name: "Bob" } # limit 10
