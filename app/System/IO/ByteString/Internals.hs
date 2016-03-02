module System.IO.ByteString.Internals where

import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import Data.Text (unpack, pack)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)

decode :: ByteString -> IO String
decode = either throwIO (pure . unpack) . decodeUtf8'

decodePair :: (ByteString, ByteString) -> IO (String, String)
decodePair (s1,s2) = (,) <$> decode s1 <*> decode s2

encode :: String -> ByteString
encode = encodeUtf8 . pack
