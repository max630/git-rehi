module System.Environment.ByteString (
    getArgs, getEnv
  ) where

import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import Data.Text (unpack,pack)
import Data.Text.Encoding (decodeUtf8',encodeUtf8)

import qualified System.Environment as SE

getArgs = SE.getArgs >>= mapM (pure . encodeUtf8 . pack)

getEnv v = decode v >>= SE.getEnv >>= (pure . encodeUtf8 . pack)

decode :: ByteString -> IO String
decode = either throwIO (pure . unpack) . decodeUtf8'


