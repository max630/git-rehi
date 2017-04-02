{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Rehi.Utils.ArgList where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (split)
import Data.Monoid (Monoid)

import GHC.Exts(IsString, fromString, IsList, Item, fromList, toList)

newtype ArgList = ArgList { getArgList :: [ByteString] } deriving (Show,Monoid)

instance IsString ArgList where
  fromString = ArgList . split ' ' . fromString

instance IsList ArgList where
  type Item ArgList = ByteString
  fromList = ArgList
  toList = getArgList
