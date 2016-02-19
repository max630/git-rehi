{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Rehi.Utils where

import Prelude hiding (putStrLn,putStr,writeFile,readFile)

import Control.Monad.Catch (MonadMask,finally)
import Control.Monad.IO.Class (liftIO,MonadIO)
import Control.Monad.Trans.Writer(execWriterT)
import Control.Monad.Writer(tell)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import System.File.ByteString (withFile,openFile)
import System.Exit (ExitCode(ExitSuccess))
import System.IO(Handle,hClose,IOMode(WriteMode,AppendMode,ReadMode),hSetBinaryMode)
import System.Process.ByteString (system,shell,std_out,createProcess,StdStream(CreatePipe),waitForProcess)

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as BC

equalWith :: (a -> b -> Bool) -> [a] -> [b] -> Bool
equalWith _ [] [] = True
equalWith f (x : xs) (y : ys) = if f x y then equalWith f xs ys else False
equalWith _ _ _ = False

index_only :: (Eq a, Enum i, Num i) => a -> [a] -> i
index_only x ys = fromMaybe (error "index_only: not found") (foldl' step Nothing $ zip [0 .. ] ys)
  where
    step prev (n, y) | x == y = case prev of { Nothing -> Just n; Just _ -> error "index_only: duplicate" }
    step prev _ = prev

run_command :: ByteString.ByteString -> IO ()
run_command s = system s >>= \case
  ExitSuccess -> pure ()
  err -> fail ("Command " <> show s <> " failed: " <> show err) -- TODO: allow non-zero and handle it in clients

readPopen :: ByteString.ByteString -> IO ByteString.ByteString
readPopen cmd = do
  (Nothing, Just out, Nothing, pHandle) <- createProcess (shell cmd){ std_out = CreatePipe }
  finally
    (fmap trim $ ByteString.hGetContents out)
    (waitForProcess pHandle)

mapCmdLinesM :: (MonadIO m, MonadMask m) => (ByteString.ByteString -> m a) -> ByteString.ByteString -> Char -> m ()
mapCmdLinesM func cmd sep = do
  (Nothing, Just out, Nothing, p) <- liftIO $ createProcess (shell cmd){ std_out = CreatePipe}
  finally
    (mapHandleLinesM_ func sep out)
    (liftIO $ waitForProcess p)

mapFileLinesM :: (MonadIO m, MonadMask m) => (ByteString.ByteString -> m ()) -> ByteString.ByteString -> Char -> m ()
mapFileLinesM func path sep = do
  h <- liftIO $ openFile path ReadMode
  liftIO $ hSetBinaryMode h True
  finally
    (mapHandleLinesM_ func sep h)
    (liftIO $ hClose h)

mapHandleLinesM_ :: MonadIO m => (ByteString.ByteString -> m a) -> Char -> Handle -> m ()
mapHandleLinesM_ func sep handle = step "" (Just handle)
  where
    step buf hM | (chunk, rest) <- BC.span (/= sep) buf, not(BC.null rest) = func chunk >> step (BC.drop 1 rest) hM
    step buf (Just h) = do
      next <- liftIO $ ByteString.hGetSome h 2048
      if BC.null next
        then do
          liftIO $ hClose h
          step buf Nothing
        else step (buf <> next) (Just h)
    step "" Nothing = pure ()
    step buf Nothing = func buf >> pure ()

command_lines :: ByteString.ByteString -> Char -> IO [ByteString.ByteString]
command_lines cmd sep = execWriterT $ mapCmdLinesM (tell . (: [])) cmd sep

modifySnd :: (b -> c) -> (a, b) -> (a, c)
modifySnd f (x, y) = (x, f y)

trim :: ByteString.ByteString -> ByteString.ByteString
trim = fst . (ByteString.spanEnd space) . ByteString.dropWhile space
  where
    space = (`ByteString.elem` " \t\n\r")

writeFile :: ByteString.ByteString -> ByteString.ByteString -> IO ()
writeFile path content = withFile path WriteMode (\h -> BC.hPut h content)

appendToFile :: ByteString.ByteString -> ByteString.ByteString -> IO ()
appendToFile path content = withFile path AppendMode (\h -> BC.hPut h content)

whenM :: Monad m => m Bool -> m () -> m ()
whenM p f = ifM p f (pure ())

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM p f = ifM p (pure ()) f

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p ft ff = p >>= \pv -> if pv then ft else ff
