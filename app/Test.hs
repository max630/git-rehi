{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Test where

import Rehi hiding (main)
import Rehi.ArgList (ArgList(ArgList))
import Rehi.Regex (regex_split)
import Rehi.GitTypes (Hash(Hash), hashString)
import Rehi.IO (getTemporaryDirectory,removeFile,openBinaryTempFile,readBinaryFile)
import Rehi.Utils (popen_lines)

import Test.HUnit (test,(~:),(~=?),(~?=),(@=?),(@?=),(@?),(~?),(@?),runTestTT,assertFailure)

import Prelude hiding (putStrLn,putStr,writeFile,readFile)

import Control.Exception (ErrorCall(ErrorCall))
import Control.Monad.Catch(finally,catch)
import Control.Monad.State(execState)
import Data.ByteString.Builder (toLazyByteString, word64HexFixed, string7)
import Data.ByteString.Lazy (toStrict)
import Data.List (isPrefixOf)
import Data.Monoid ((<>))
import System.IO(hClose)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M

main = runTestTT  allTests

allTests = test [ "regex" ~:
                    [ "split keeps last " ~: regex_split "a b c" " " ~?= ["a", "b", "c"] ]
                , "find_sequence" ~:
                    [ "linear" ~: test_findseq [(1,[2]),(2,[3]),(3,[4])] 4 1 [] ~?= [h 3, h 2, h 1]
                    , "diamond" ~: test_findseq [(4,[2,3]),(2,[1]),(3,[1]),(1,[0])] 0 4 [] ~?= map h [1,3,2,4]
                    , "simple_branch" ~: test_findseq [(1,[2,3]),(2,[3,5])] 2 1 [] ~?= [h 1]
                    , "shortest" ~: test_findseq [(1,[2,3]),(2,[6]),(3,[4]),(4,[6]),(6,[7,10])] 6 1 [] ~?= map h [2,1]
                    , "through" ~: test_findseq [(1,[2,3]),(2,[6]),(3,[4]),(4,[6]),(6,[7,10])] 6 1 [4] ~?= map h [4,3,1]
                    , "parallel_throughs" ~:
                        mustError
                          (mconcat $ map hashString $ test_findseq [(1,[2,3]),(2,[6]),(3,[4]),(4,[6]),(6,[7,10])] 6 1 [2,4])
                          (== "No path found")
                    , "inner merge" ~: test_findseq [(1,[2,5]),(2,[3]),(3,[4]),(4,[]),(5,[6]),(6,[3,7]),(7,[4])] 4 1 [] ~?= map h [3,6,5,2,1]
                    , "inner merge through" ~: test_findseq [(1,[2,5]),(2,[3]),(3,[4]),(4,[]),(5,[6]),(6,[3,7]),(7,[4])] 4 1 [7] ~?= map h [7,6,5,1] ]
                , "build_rebase_sequence" ~:
                    [ "1" ~: brs1 ~?= [Pick (hashes !! 0)]
                    , "2" ~: brs2 ~?= []
                    , "3" ~: brs3 ~?= [p 1
                                      , Mark "tmp_1"
                                      , p 3
                                      , Mark "tmp_2"
                                      , Reset "@tmp_1"
                                      , p 2
                                      , Merge (Just (hashes !! 4)) ["HEAD", "@tmp_2"] False False]
                    , "inner_merge_after_merge " ~: test_brs [(1,[2,3]),(2,[4]),(3,[4]),(4,[5,6]),(5,[7]),(6,[8])] 5 1 []
                                                      ~?= [ Merge (Just (hashes !! 4)) ["HEAD",hashes !! 6] False False
                                                          , Mark "tmp_1", p 3, Mark "tmp_2", Reset "@tmp_1", p 2
                                                          , Merge (Just (hashes !! 1)) ["HEAD","@tmp_2"] False False ] ]
                , "parse_cli_p1" ~: p1 ~?= Run "origin/b4" Nothing [] (Just "origin/base") Nothing True
                , "parse_cli" ~: test_parse_cli
                , "parse_todo" ~:
                    [ "1" ~: tp1 >>= (@?= [Merge (Just "f1") ["HEAD", "f2"] False False])
                    , "2" ~: tp2 >>= (@?= [Edit "316bf9c"])
                    , "3" ~: tp3 >>= (@?= [Pick "316bf9c", Comment "test-comment\n"])]
                , "save_todo" ~:
                    [ "1" ~: s1 >>= (@?= "merge --ours -c 1 HEAD,2 ???\n") ]
                , "parse_commit_line" ~:
                    [ "1" ~: commitsRefs pl1 M.! "9ac82f5" ~?= Hash "9ac82f5327efe63acb5267d9d55edbd8576d9d26"
                    , "2" ~: entryBody (commitsByHash pl1 M.! Hash "9ac82f5327efe63acb5267d9d55edbd8576d9d26") ~?= "Merge remote-tracking branch 'origin/b2'\n\nresolve conflict also\n"]
                , "comments_from_string" ~:
                    [ "normal" ~: comments_from_string "a\nb\n" 0 ~?= [UserComment "a", UserComment "b"]
                    , "pending line" ~: comments_from_string "a\nb" 0 ~?= [UserComment "a", UserComment "b"] ]
                , test_popen ]
  where
    p n = Pick (hashes !! n)
    h n = Hash (hashes !! n)

hashes = map (B.reverse . toStrict . toLazyByteString . (string7 (replicate 24 '0') <>) . word64HexFixed) [0 ..]

i2c (n,ps) = (Hash h, Entry h (Hash h) h phs (Hash h) h)
  where
    h = hashes !! n
    phs = map (Hash . (hashes !!)) ps

noCommits = Commits M.empty M.empty

test_brs commits from to throughs =
  build_rebase_sequence
    (Commits M.empty (M.fromList $ map i2c commits))
    (Hash (hashes !! from))
    (Hash (hashes !! to))
    (map (Hash . (hashes !!)) throughs)

test_findseq commits from to throughs =
  find_sequence
    (M.fromList $ map i2c commits)
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
  let c = Commits M.empty M.empty
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
  save_todo [Merge (Just "1") ["HEAD","2"] True False] f commitsEmpty
  readBinaryFile f

pl1 = execState
        (git_parse_commit_line ("9ac82f5327efe63acb5267d9d55edbd8576d9d26:9ac82f5:a93dcfc33f5b7639a9e7c96bfeec0831451a918f:"
                                <> "97277bafae875c930ea7c4a338a82073c897f7f0 76dee8a19ec9fddea0a02d99b0d1e00b1ef1caba:"
                                <> "Merge remote-tracking branch 'origin/b2'\n\nresolve conflict also\n"))
        noCommits

mustError expr p_msg =
  catch
    (seq expr (assertFailure "Must have fail"))
    (\case { ErrorCall m | p_msg m -> pure (); err -> (assertFailure (show err)) })

test_parse_cli =
  [ "regular" ~:
    [ parse_cli ["a"] ~?= Run "a" Nothing [] Nothing Nothing False
    , parse_cli ["a","c"] ~?= Run "a" Nothing [] Nothing (Just "c") False
    , parse_cli ["a","b..d","c"] ~?= Run "a" (Just "b") [] (Just "d") (Just "c") False
    , parse_cli ["a","b..","c"] ~?= Run "a" (Just "b") [] Nothing (Just "c") False
    , parse_cli ["a","..d","c"] ~?= Run "a" Nothing [] (Just "d") (Just "c") False
    , parse_cli ["a","b..e..d","c"] ~?= Run "a" (Just "b") ["e"] (Just "d") (Just "c") False
    , parse_cli ["a","..e..","c"] ~?= Run "a" Nothing ["e"] Nothing (Just "c") False
    , parse_cli ["a","..e.."] ~?= Run "a" Nothing ["e"] Nothing Nothing False
    , parse_cli ["a","b..e..f..d","c"] ~?= Run "a" (Just "b") ["e","f"] (Just "d") (Just "c") False
  , "failures" ~:
    [ mustError (runThroughs $ parse_cli ["a", "b...d"]) (isPrefixOf "Invalid source spec:") ] ] ]


test_popen =
  test [ (popen_lines "/bin/echo" "aaa" '\n' >>= (pure . (== ["aaa"]))) @? "popen_lines" ]
