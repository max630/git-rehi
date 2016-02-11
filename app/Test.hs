{-# LANGUAGE OverloadedStrings #-}
module Test where

import Rehi

import Test.HUnit (test,(~:),(~=?),(~?=),(@=?),(@?=),(@?),runTestTT)

import Prelude hiding (putStrLn,putStr,writeFile,readFile)

import Control.Monad.Catch(finally)
import Control.Monad.State(execState)
import Data.ByteString.Builder (toLazyByteString, word64HexFixed, string7)
import Data.ByteString.Lazy (toStrict)
import Data.Monoid ((<>))
import System.Directory.ByteString (getTemporaryDirectory,removeFile)
import System.File.ByteString (openBinaryTempFile,readFile)
import System.IO(hClose)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M

main = undefined

allTests = test [ "regex" ~:
                    [ "split keeps last " ~: regex_split "a b c" " " ~?= ["a", "b", "c"] ]
                , "build_rebase_sequence" ~:
                    [ "1" ~: brs1 ~?= [Pick (hashes !! 0)]
                    , "2" ~: brs2 ~?= []
                    , "3" ~: brs3 ~?= [p 1
                                      , Mark "tmp_1"
                                      , p 3
                                      , Mark "tmp_2"
                                      , Reset "@tmp_1"
                                      , p 2
                                      , Merge (Just (hashes !! 4)) ["HEAD", "@tmp_2"] False False] ]
                , "parse_cli" ~: p1 ~?= Run "origin/b4" Nothing [] (Just "origin/base") Nothing True
                , "parse_todo" ~:
                    [ "1" ~: tp1 >>= (@?= [Merge (Just "f1") ["HEAD", "f2"] False False])
                    , "2" ~: tp2 >>= (@?= [Edit "316bf9c"])
                    , "3" ~: tp3 >>= (@?= [Pick "316bf9c", Comment "test-comment\n"])]
                , "save_todo" ~:
                    [ "1" ~: s1 >>= (@?= "merge --ours -c 1 HEAD,2 ???\n") ]
                , "parse_commit_line" ~:
                    [ "1" ~: stateRefs pl1 M.! "9ac82f5" ~?= Hash "9ac82f5327efe63acb5267d9d55edbd8576d9d26"
                    , "2" ~: entryBody (stateByHash pl1 M.! Hash "9ac82f5327efe63acb5267d9d55edbd8576d9d26") ~?= "Merge remote-tracking branch 'origin/b2'\n\nresolve conflict also\n"]
                , "comments_from_string" ~:
                    [ "normal" ~: comments_from_string "a\nb\n" 0 ~?= [UserComment "a", UserComment "b"]
                    , "pending line" ~: comments_from_string "a\nb" 0 ~?= [UserComment "a", UserComment "b"] ]
                    ]
  where
    p n = Pick (hashes !! n)

hashes = map (B.reverse . toStrict . toLazyByteString . (string7 (replicate 24 '0') <>) . word64HexFixed) [0 ..]

i2c (n,ps) = (Hash h, Entry h (Hash h) h phs (Hash h) h)
  where
    h = hashes !! n
    phs = map (Hash . (hashes !!)) ps

noCommits = Commits Sync M.empty M.empty M.empty

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

-- p 1, mark, p 2, mark, reset, p 3, merge
brs3 = test_brs [(1, [0]),(2, [1]),(3,[1]),(4,[2,3])] 0 4 []

-- Run "origin/b4" Nothing [] (Just "origin/base") Nothing True
p1 = parse_cli ["-i","origin/b4","..origin/base"]

-- merge parse
tp1 = runParseTodo "merge -c f1 HEAD,f2 Test subject\n"

tp2 = runParseTodo "edit 316bf9c init haskell project\n"

tp3 = runParseTodo "pick 316bf9c\ncomment {{{\ntest-comment\n}}}\n"

runParseTodo content = withTestFile $ \f h -> do
  let c = Commits Sync M.empty M.empty M.empty
  finally
    (BC.hPut h content)
    (hClose h)
  read_todo f c

withTestFile func = do
  d <- getTemporaryDirectory
  (f,h) <- openBinaryTempFile d "test.txt"
  finally
    (func f h)
    (removeFile f)

s1 = withTestFile $ \f h -> do
  hClose h
  save_todo [Merge (Just "1") ["HEAD","2"] True False] f noCommits
  readFile f

pl1 = execState
        (git_parse_commit_line ("9ac82f5327efe63acb5267d9d55edbd8576d9d26:9ac82f5:a93dcfc33f5b7639a9e7c96bfeec0831451a918f:"
                                <> "97277bafae875c930ea7c4a338a82073c897f7f0 76dee8a19ec9fddea0a02d99b0d1e00b1ef1caba:"
                                <> "Merge remote-tracking branch 'origin/b2'\n\nresolve conflict also\n"))
        noCommits
