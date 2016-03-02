module System.File.ByteString (
    readFile,
    withFile,
    openFile,
    openBinaryTempFile
  ) where

import Prelude hiding (readFile)

import Control.Monad (join)
import Data.ByteString (ByteString)
import System.IO.ByteString.Internals (decode, encode)

import qualified Data.ByteString as B
import qualified System.IO as SI

withFile :: ByteString -> SI.IOMode -> (SI.Handle -> IO ()) -> IO ()
withFile p m f = join $ SI.withFile <$> decode p <*> pure m <*> pure f

readFile p = decode p >>= B.readFile

openFile :: ByteString -> SI.IOMode -> IO SI.Handle
openFile p m = join $ SI.openFile <$> decode p <*> pure m

openBinaryTempFile :: ByteString -> ByteString -> IO (ByteString, SI.Handle)
openBinaryTempFile dir fn = do
  dir <- decode dir
  fn <- decode fn
  (p, h) <- SI.openBinaryTempFile dir fn
  pure (encode p, h)
