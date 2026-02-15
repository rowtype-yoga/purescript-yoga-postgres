module Yoga.Postgres.UUID where

import Prelude

import Effect (Effect)
import Yoga.Postgres.Schema (UUID(..))

foreign import genUUIDv7Impl :: Effect String

genUUIDv7 :: Effect UUID
genUUIDv7 = do
  s <- genUUIDv7Impl
  pure (UUID s)

foreign import genUUIDv4Impl :: Effect String

genUUIDv4 :: Effect UUID
genUUIDv4 = do
  s <- genUUIDv4Impl
  pure (UUID s)
