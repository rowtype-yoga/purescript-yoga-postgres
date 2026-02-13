module Test.Postgres.Client where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Type.Function (type (#))
import Type.Proxy (Proxy(..))
import Yoga.Postgres as PG
import Yoga.Postgres.Client
import Yoga.Postgres.Schema (Table, PrimaryKey, AutoIncrement, Unique, ForeignKey, References)

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Table definitions with ForeignKey constraints
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

type UsersTable = Table "users"
  ( id :: Int # PrimaryKey # AutoIncrement
  , name :: String
  , email :: String # Unique
  , age :: Maybe Int
  )

usersTable :: Proxy UsersTable
usersTable = Proxy

type PostsTable = Table "posts"
  ( id :: Int # PrimaryKey # AutoIncrement
  , title :: String
  , body :: String
  , user_id :: Int # ForeignKey "users" References "id"
  )

postsTable :: Proxy PostsTable
postsTable = Proxy

type CommentsTable = Table "comments"
  ( id :: Int # PrimaryKey # AutoIncrement
  , text :: String
  , post_id :: Int # ForeignKey "posts" References "id"
  , user_id :: Int # ForeignKey "users" References "id"
  )

commentsTable :: Proxy CommentsTable
commentsTable = Proxy

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Type-level tests: type annotations prove correctness
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

typedFindAll
  :: PG.Connection
  -> Aff (Array { id :: Int, name :: String, email :: String, age :: Maybe Int })
typedFindAll conn = findAll usersTable conn

typedFindById
  :: PG.Connection
  -> Aff (Maybe { id :: Int, name :: String, email :: String, age :: Maybe Int })
typedFindById conn = findById usersTable conn 42

typedCreate
  :: PG.Connection
  -> Aff (Maybe { id :: Int, name :: String, email :: String, age :: Maybe Int })
typedCreate conn = create usersTable conn { name: "Alice", email: "alice@example.com" }

typedCreateWithOptional
  :: PG.Connection
  -> Aff (Maybe { id :: Int, name :: String, email :: String, age :: Maybe Int })
typedCreateWithOptional conn =
  create usersTable conn { name: "Alice", email: "alice@example.com", age: Just 25 }

typedUpdateById
  :: PG.Connection
  -> Aff (Maybe { id :: Int, name :: String, email :: String, age :: Maybe Int })
typedUpdateById conn = updateById usersTable conn 1 { name: "Bob" }

typedUpdateByIdMultiple
  :: PG.Connection
  -> Aff (Maybe { id :: Int, name :: String, email :: String, age :: Maybe Int })
typedUpdateByIdMultiple conn =
  updateById usersTable conn 1 { name: "Bob", age: Just 30 }

typedDeleteById
  :: PG.Connection
  -> Aff Int
typedDeleteById conn = deleteById usersTable conn 1

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Belongs-to join: type annotations prove nested result shape
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

typedFindWith
  :: PG.Connection
  -> Aff (Array { posts :: { id :: Int, title :: String, body :: String, user_id :: Int }
                , users :: { id :: Int, name :: String, email :: String, age :: Maybe Int }
                })
typedFindWith conn = findWith postsTable usersTable conn

typedFindByIdWith
  :: PG.Connection
  -> Aff (Maybe { posts :: { id :: Int, title :: String, body :: String, user_id :: Int }
                , users :: { id :: Int, name :: String, email :: String, age :: Maybe Int }
                })
typedFindByIdWith conn = findByIdWith postsTable usersTable conn 1

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
-- Has-many include: parent row extended with nested children
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

typedFindIncluding
  :: PG.Connection
  -> Aff (Array { id :: Int, name :: String, email :: String, age :: Maybe Int
                , posts :: Array { id :: Int, title :: String, body :: String, user_id :: Int }
                })
typedFindIncluding conn = findIncluding @"posts" usersTable postsTable conn

typedFindByIdIncluding
  :: PG.Connection
  -> Aff (Maybe { id :: Int, name :: String, email :: String, age :: Maybe Int
                , posts :: Array { id :: Int, title :: String, body :: String, user_id :: Int }
                })
typedFindByIdIncluding conn = findByIdIncluding @"posts" usersTable postsTable conn 42

-- Comments belong to posts
typedCommentsWithPost
  :: PG.Connection
  -> Aff (Array { comments :: { id :: Int, text :: String, post_id :: Int, user_id :: Int }
                , posts :: { id :: Int, title :: String, body :: String, user_id :: Int }
                })
typedCommentsWithPost conn = findWith commentsTable postsTable conn

-- Posts including comments
typedPostsIncludingComments
  :: PG.Connection
  -> Aff (Array { id :: Int, title :: String, body :: String, user_id :: Int
                , comments :: Array { id :: Int, text :: String, post_id :: Int, user_id :: Int }
                })
typedPostsIncludingComments conn = findIncluding @"comments" postsTable commentsTable conn
