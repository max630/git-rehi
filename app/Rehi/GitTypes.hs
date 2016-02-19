module Rehi.GitTypes where

import Data.ByteString(ByteString)

newtype Hash = Hash { hashString :: ByteString } deriving (Eq, Ord, Show)
