module System.Directory.ByteString (
    createDirectory,
    doesFileExist,
    removeDirectoryRecursive,
    removeFile
  ) where

import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8')

import qualified System.Directory as SD

createDirectory p = SD.createDirectory =<< decode p

doesFileExist p = SD.doesFileExist =<< decode p

removeDirectoryRecursive p = SD.removeDirectoryRecursive =<< decode p

removeFile p = SD.removeFile =<< decode p

decode :: ByteString -> IO String
decode = either throwIO (pure . unpack) . decodeUtf8'
