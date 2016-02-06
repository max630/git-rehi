module System.Directory.ByteString (
    createDirectory,
    doesFileExist,
    doesDirectoryExist,
    getTemporaryDirectory,
    removeDirectoryRecursive,
    removeFile
  ) where

import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import Data.Text (unpack,pack)
import Data.Text.Encoding (decodeUtf8',encodeUtf8)

import qualified System.Directory as SD

createDirectory p = SD.createDirectory =<< decode p

doesFileExist p = SD.doesFileExist =<< decode p

doesDirectoryExist p = SD.doesDirectoryExist =<< decode p

getTemporaryDirectory = SD.getTemporaryDirectory >>= (pure . encodeUtf8 . pack)

removeDirectoryRecursive p = SD.removeDirectoryRecursive =<< decode p

removeFile p = SD.removeFile =<< decode p

decode :: ByteString -> IO String
decode = either throwIO (pure . unpack) . decodeUtf8'
