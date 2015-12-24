module System.Process.ByteString (
    CreateProcess(..),
    CmdSpec(..),
    ProcessHandle,
    StdStream(..),
    createProcess,
    shell,
    system,
    waitForProcess,getProcessExitCode,terminateProcess
  ) where

import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8')
import System.Exit (ExitCode)
import System.IO (Handle)
import System.Posix.ByteString (RawFilePath)
import System.Process (StdStream(..),ProcessHandle,waitForProcess,getProcessExitCode,
                       terminateProcess)

import qualified System.Process as SP

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

decodePair (s1,s2) = (,) <$> decode s1 <*> decode s2

decodeCreateProcess (CreateProcess cmdspec cwd env std_in std_out std_err close_fds create_group delegate_ctlc)
  = SP.CreateProcess <$> decodeCmdSpec cmdspec <*> mapM decode cwd <*> mapM (mapM decodePair) env
                    <*> pure std_in <*> pure std_out <*> pure std_err <*> pure close_fds
                    <*> pure create_group <*> pure delegate_ctlc

data CreateProcess = CreateProcess {
    cmdspec :: CmdSpec,
    cwd :: Maybe RawFilePath,
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
    | RawCommand RawFilePath [ByteString]

decode :: ByteString -> IO String
decode bs = case decodeUtf8' bs of
  Left e -> throwIO e
  Right s -> pure $ unpack s

system :: ByteString -> IO ExitCode
system s = decode s >>= SP.system
