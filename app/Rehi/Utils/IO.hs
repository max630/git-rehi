{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Rehi.Utils.IO (
    callCommand,
    callProcess,
    createDirectory,
    copyFile,
    doesDirectoryExist,
    doesFileExist,
    getArgs,
    getEnv,
    getTemporaryDirectory,
    initProgram,
    lookupEnv,
    openBinaryFile,
    openBinaryTempFile,
    proc,
    readBinaryFile,
    readCommand,
    removeDirectoryRecursive,
    removeFile,
    shell,
    system,
    withBinaryFile,
    decode,
  ) where

import Control.Monad (join)
import Data.ByteString (ByteString)
import Data.Maybe (isJust)
import GHC.Foreign (peekCStringLen, withCStringLen)
import GHC.IO.Encoding (setFileSystemEncoding)
import GHC.IO.Encoding.Failure (CodingFailureMode(RoundtripFailure))
import GHC.IO.Encoding.UTF8 (mkUTF8)
import System.Exit (ExitCode)

import qualified Data.ByteString as B
import qualified System.Environment as SE
import qualified System.Directory as SD
import qualified System.IO as SI
import qualified System.Process as SP

#ifdef mingw32_HOST_OS
import Rehi.Utils.Win32bits (getFileNameInformation)
import Rehi.Utils.Regex (regex_match)
import qualified Graphics.Win32.Misc as WM
import qualified System.Win32.File as WF
#endif

withBinaryFile :: ByteString -> SI.IOMode -> (SI.Handle -> IO ()) -> IO ()
withBinaryFile p m f = join $ SI.withFile <$> decode p <*> pure m <*> pure f

readBinaryFile p = decode p >>= B.readFile

openBinaryFile :: ByteString -> SI.IOMode -> IO SI.Handle
openBinaryFile p m = join $ SI.openFile <$> decode p <*> pure m

openBinaryTempFile :: ByteString -> ByteString -> IO (ByteString, SI.Handle)
openBinaryTempFile dir fn = do
  dir <- decode dir
  fn <- decode fn
  (p, h) <- SI.openBinaryTempFile dir fn
  (, h) <$> encode p

createDirectory p = SD.createDirectory =<< decode p

copyFile :: ByteString -> ByteString -> IO ()
copyFile old new = join $ SD.copyFile <$> decode old <*> decode new

doesFileExist p = SD.doesFileExist =<< decode p

doesDirectoryExist p = SD.doesDirectoryExist =<< decode p

getTemporaryDirectory :: IO ByteString
getTemporaryDirectory = SD.getTemporaryDirectory >>= encode

removeDirectoryRecursive p = SD.removeDirectoryRecursive =<< decode p

removeFile p = SD.removeFile =<< decode p

readCommand :: ByteString -> IO ByteString
readCommand cmd = do
  cmdS <- decode cmd
  SP.readCreateProcess (SP.shell cmdS) "" >>= encode

proc :: ByteString -> [ByteString] -> IO SP.CreateProcess
proc exe args = SP.proc <$> decode exe <*> mapM decode args

shell :: ByteString -> IO SP.CreateProcess
shell cmd = do
  cmdS <- decode cmd
  pure (SP.shell cmdS)

system :: ByteString -> IO ExitCode
system s = decode s >>= SP.system

callCommand :: ByteString -> IO ()
callCommand s = decode s >>= SP.callCommand

callProcess :: ByteString -> [ByteString] -> IO ()
callProcess exe args = join $ SP.callProcess <$> decode exe <*> mapM decode args

getArgs :: IO [ByteString]
getArgs = SE.getArgs >>= mapM encode

getEnv :: ByteString -> IO ByteString
getEnv v = decode v >>= SE.getEnv >>= encode

lookupEnv :: ByteString -> IO (Maybe ByteString)
lookupEnv v = decode v >>= SE.lookupEnv >>= mapM encode

decode :: ByteString -> IO String
decode bs = B.useAsCStringLen bs (peekCStringLen utf8_roundtrip)

decodePair :: (ByteString, ByteString) -> IO (String, String)
decodePair (s1,s2) = (,) <$> decode s1 <*> decode s2

encode :: String -> IO ByteString
encode s = withCStringLen utf8_roundtrip s B.packCStringLen

utf8_roundtrip = mkUTF8 RoundtripFailure

initProgram :: IO ()
initProgram = do
  setFileSystemEncoding utf8_roundtrip
#ifdef mingw32_HOST_OS
  stdoutH <- WM.getStdHandle WM.sTD_OUTPUT_HANDLE
  stdoutT <- WF.getFileType stdoutH
  if stdoutT == WF.fILE_TYPE_PIPE
    then do
      name <- encode =<< getFileNameInformation stdoutH
      case regex_match "^\\\\Device\\\\NamedPipe\\\\msys-.*pty.*-master$" name of
        Just _ -> SI.hSetBuffering SI.stdout SI.NoBuffering
        Nothing -> pure ()
    else pure ()
#else
  pure ()
#endif
