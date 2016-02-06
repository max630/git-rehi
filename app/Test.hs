{-# LANGUAGE OverloadedStrings #-}
module Test where

import Rehi

import Control.Monad.Catch(finally)
import Data.ByteString.Builder (toLazyByteString, word64HexFixed, string7)
import Data.ByteString.Lazy (toStrict)
import Data.Monoid ((<>))
import System.Directory.ByteString (getTemporaryDirectory,removeFile)
import System.File.ByteString (openBinaryTempFile)
import System.IO(hClose)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M

main = undefined

hashes = map (B.reverse . toStrict . toLazyByteString . (string7 (replicate 24 '0') <>) . word64HexFixed) [0 ..]

i2c (n,ps) = (Hash h, Entry h (Hash h) h phs (Hash h) h)
  where
    h = hashes !! n
    phs = map (Hash . (hashes !!)) ps

test_brs commits from to throughs =
  build_rebase_sequence
    (Commits Sync M.empty M.empty (M.fromList $ map i2c commits))
    (Hash (hashes !! from))
    (Hash (hashes !! to))
    (map (Hash . (hashes !!)) throughs)

-- pick 0
brs1 = test_brs [(0, [1])] 1 0 []

-- empty
brs2 = test_brs [(1, [0]),(2, [1])] 2 2 []

-- p 1, mark, p 2, reset, p 3, merge
brs3 = test_brs [(1, [0]),(2, [1]),(3,[1]),(4,[2,3])] 0 4 []

-- Run "origin/b4" Nothing [] (Just "origin/base") Nothing True
p1 = parse_cli ["-i","origin/b4","..origin/base"]

-- merge parse
tp1 = do
  let c = Commits Sync M.empty M.empty M.empty
  d <- getTemporaryDirectory
  (f,h) <- openBinaryTempFile d "merge-todo.txt"
  finally
    (do
      finally
        (BC.hPut h "merge -c f1 HEAD,f2 Test subject\n")
        (hClose h)
      read_todo f c)
    (removeFile f)
