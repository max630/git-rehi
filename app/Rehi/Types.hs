module Rehi.Types where

import Data.ByteString(ByteString)
import Control.Monad.Catch(Exception)

import qualified Data.Map.Strict as Map

import Rehi.Git.Types (Hash(Hash),hashString)

data Step =
    Pick ByteString
  | Fixup ByteString
  | Edit ByteString
  | Exec ByteString
  | Comment ByteString
  | Merge { mergeRef :: Maybe ByteString, mergeParents :: [ByteString], mergeOurs :: Bool, mergeNoff :: Bool }
  | Mark ByteString
  | Reset ByteString
  | UserComment ByteString
  | TailPickWithComment ByteString ByteString
  deriving (Show, Eq)

data Commits = Commits {
    commitsRefs :: Map.Map ByteString Hash
  , commitsByHash :: Map.Map Hash Entry
  } deriving Show

data Entry = Entry {
    entryAHash :: ByteString
  , entryHash :: Hash
  , entrySubject :: ByteString
  , entryParents :: [Hash]
  , entryTree :: Hash
  , entryBody :: ByteString
  } deriving Show

newtype EditError = EditError ByteString deriving Show

instance Exception EditError

