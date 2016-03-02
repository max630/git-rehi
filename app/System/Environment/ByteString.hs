module System.Environment.ByteString (
    getArgs, getEnv, lookupEnv
  ) where

import System.IO.ByteString.Internals (decode, encode)

import qualified System.Environment as SE

getArgs = SE.getArgs >>= mapM (pure . encode)

getEnv v = decode v >>= SE.getEnv >>= (pure . encode)

lookupEnv v = decode v >>= SE.lookupEnv >>= (pure . fmap encode)
