{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Rehi.GitCommands where

import Data.Maybe (maybe)
import Data.Monoid ((<>))

import qualified Data.ByteString as B

import Rehi.ArgList (ArgList(ArgList), getArgList)
import Rehi.IO (readCommand,callProcess)
import Rehi.Utils (equalWith, index_only, mapFileLinesM, modifySnd,
                   trim, writeFile, appendToFile, whenM, unlessM, ifM, popen_lines)
import Rehi.Regex (regex_match, regex_match_with_newlines, regex_match_all, regex_split)
import Rehi.GitTypes

fixup :: B.ByteString -> IO ()
fixup ref = do
  git ("cherry-pick --allow-empty --allow-empty-message --no-commit" <> [ref])
  git "commit --amend --reset-author --no-edit"

reset :: B.ByteString -> IO ()
reset ref = git ("reset --hard" <> [ref])

checkout_detached :: B.ByteString -> IO ()
checkout_detached ref =  git ("checkout --quiet --detach" <> [ref])

checkout_here :: B.ByteString -> IO ()
checkout_here branch = git ("checkout -B" <> [branch])

checkout_force :: B.ByteString -> IO ()
checkout_force branch = git ("checkout -f" <> [branch])

verify_clean :: IO ()
verify_clean = do
  readCommand "git rev-parse --verify HEAD"
  git "update-index --ignore-submodules --refresh"
  git "diff-files --quiet --ignore-submodules"

commit :: Maybe B.ByteString -> IO ()
commit refMb = git ("commit" <> maybe [] (\r -> ["-c", r]) refMb)

commit_amend :: IO ()
commit_amend = git "commit --amend"

commit_amend_msgFile :: B.ByteString -> IO ()
commit_amend_msgFile path = git ("commit --amend -F" <> [path])

commit_refMsgOnly :: B.ByteString -> IO ()
commit_refMsgOnly ref = git ("commit -C" <> [ref] <> "--reset-author")

cherrypick :: B.ByteString -> IO ()
cherrypick ref = git ("cherry-pick --allow-empty --allow-empty-message --ff" <> [ref])

merge :: Bool -> Bool -> Bool -> [B.ByteString] -> IO ()
merge doCommit ours noff parents = git command
  where
    command :: ArgList
    command = "merge"
                    <> (if doCommit then ["--no-edit"] else ["--no-commit"])
                    <> (if ours then ["--strategy=ours"] else [])
                    <> (if noff then ["--no-ff"] else [])
                    <> ArgList parents

git_resolve_hashes :: [B.ByteString] -> IO [Hash]
git_resolve_hashes refs = do
  hashes <- fmap (map (Hash . trim)) $ popen_lines "git" ("rev-parse" <> ArgList refs) '\n'
  if length hashes == length refs
    then pure hashes
    else error "Hash number does not match"

edit_config_file :: B.ByteString -> IO ()
edit_config_file path = git ("config --edit --file" <> [path])

git :: ArgList -> IO ()
git al = callProcess "git" (getArgList al)
