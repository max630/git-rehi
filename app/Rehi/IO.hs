{-# LANGUAGE TupleSections #-}
module Rehi.IO (
    createDirectory,
    createProcess,
    doesDirectoryExist,
    doesFileExist,
    getArgs,
    getEnv,
    getTemporaryDirectory,
    initEncoding,
    lookupEnv,
    openBinaryFile,
    openBinaryTempFile,
    readBinaryFile,
    removeDirectoryRecursive,
    removeFile,
    shell,
    std_out,
    system,
    withBinaryFile,
  ) where

import Control.Monad (join)
import Data.ByteString (ByteString)
import GHC.Foreign (peekCStringLen, withCStringLen)
import GHC.IO.Encoding (setFileSystemEncoding)
import GHC.IO.Encoding.Failure (CodingFailureMode(RoundtripFailure))
import GHC.IO.Encoding.UTF8 (mkUTF8)
import System.Exit (ExitCode)
import System.IO (Handle)
import System.Process (StdStream(..),ProcessHandle)

import qualified Data.ByteString as B
import qualified System.Environment as SE
import qualified System.Directory as SD
import qualified System.IO as SI
import qualified System.Process as SP

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

doesFileExist p = SD.doesFileExist =<< decode p

doesDirectoryExist p = SD.doesDirectoryExist =<< decode p

getTemporaryDirectory = SD.getTemporaryDirectory >>= (pure . encode)

removeDirectoryRecursive p = SD.removeDirectoryRecursive =<< decode p

removeFile p = SD.removeFile =<< decode p

createProcess :: CreateProcess -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess cp = SP.createProcess =<< decodeCreateProcess cp

shell :: ByteString -> CreateProcess
shell str = CreateProcess { cmdspec = ShellCommand str,
                            cwd = Nothing,
                            env = Nothing,
                            std_in = Inherit,
                            std_out = Inherit,
                            std_err = Inherit,
                            close_fds = False,
                            create_group = False,
                            delegate_ctlc = False}

decodeCmdSpec (ShellCommand s) = SP.ShellCommand <$> decode s
decodeCmdSpec (RawCommand f as) = SP.RawCommand <$> decode f <*> mapM decode as

decodeCreateProcess (CreateProcess cmdspec cwd env std_in std_out std_err close_fds create_group delegate_ctlc)
  = SP.CreateProcess <$> decodeCmdSpec cmdspec <*> mapM decode cwd <*> mapM (mapM decodePair) env
                    <*> pure std_in <*> pure std_out <*> pure std_err <*> pure close_fds
                    <*> pure create_group <*> pure delegate_ctlc

data CreateProcess = CreateProcess {
    cmdspec :: CmdSpec,
    cwd :: Maybe ByteString,
    env :: Maybe [(ByteString, ByteString)],
    std_in :: StdStream,
    std_out :: StdStream,
    std_err :: StdStream,
    close_fds :: Bool,
    create_group :: Bool,
    delegate_ctlc :: Bool
}

data CmdSpec
    = ShellCommand ByteString
    | RawCommand ByteString [ByteString]


system :: ByteString -> IO ExitCode
system s = decode s >>= SP.system

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

initEncoding :: IO ()
initEncoding = setFileSystemEncoding utf8_roundtrip
