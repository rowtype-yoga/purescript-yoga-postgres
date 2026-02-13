-- EXPECT: TypesDoNotUnify
module Test.CompileFail.JoinInvalidTable where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\))
import Type.Proxy (Proxy(..))
import Yoga.Postgres.Schema

type UsersTable = Table "users"
  ( id :: Column Int (PrimaryKey /\ AutoIncrement)
  , name :: Column String None
  )

type PostsTable = Table "posts"
  ( id :: Column Int (PrimaryKey /\ AutoIncrement)
  , title :: Column String None
  , user_id :: Column Int None
  )

usersTable :: Proxy UsersTable
usersTable = Proxy

postsTable :: Proxy PostsTable
postsTable = Proxy

bad = from usersTable
  # innerJoin @"users.id = posts.user_id" postsTable
  # selectJQ @"nonexistent_table.name"
