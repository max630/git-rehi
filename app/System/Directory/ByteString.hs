module System.Directory.ByteString (
    createDirectory,
    doesFileExist,
    doesDirectoryExist,
    getTemporaryDirectory,
    removeDirectoryRecursive,
    removeFile
  ) where

import System.IO.ByteString.Internals (decode, encode)

import qualified System.Directory as SD

createDirectory p = SD.createDirectory =<< decode p

doesFileExist p = SD.doesFileExist =<< decode p

doesDirectoryExist p = SD.doesDirectoryExist =<< decode p

getTemporaryDirectory = SD.getTemporaryDirectory >>= (pure . encode)

removeDirectoryRecursive p = SD.removeDirectoryRecursive =<< decode p

removeFile p = SD.removeFile =<< decode p
