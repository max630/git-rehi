{-# LANGUAGE OverloadedStrings #-}
module Rehi.GitCommands where

import Data.Maybe (maybe)
import Data.Monoid ((<>))

import qualified Data.ByteString as B

import Rehi.IO (readCommand)
import Rehi.Utils (equalWith, index_only, run_command, readPopen, mapCmdLinesM, mapFileLinesM, modifySnd,
                   trim, writeFile, appendToFile, whenM, unlessM, ifM, command_lines)
import Rehi.Regex (regex_match, regex_match_with_newlines, regex_match_all, regex_split)
import Rehi.GitTypes

fixup :: B.ByteString -> IO ()
fixup ref = do
  run_command ("git cherry-pick --allow-empty --allow-empty-message --no-commit " <> ref)
  run_command "git commit --amend --reset-author --no-edit"

reset :: B.ByteString -> IO ()
reset ref = run_command ("git reset --hard " <> ref)

checkout_detached :: B.ByteString -> IO ()
checkout_detached ref =  run_command ("git checkout --quiet --detach " <> ref)

checkout_here :: B.ByteString -> IO ()
checkout_here branch = run_command ("git checkout -B " <> branch)

checkout_force :: B.ByteString -> IO ()
checkout_force branch = run_command ("git checkout -f " <> branch)

verify_clean :: IO ()
verify_clean = do
  readCommand "git rev-parse --verify HEAD"
  run_command "git update-index --ignore-submodules --refresh"
  run_command "git diff-files --quiet --ignore-submodules"

commit :: Maybe B.ByteString -> IO ()
commit refMb = run_command ("git commit " <> maybe "" ("-c " <>) refMb)

commit_amend :: IO ()
commit_amend = run_command "git commit --amend"

commit_amend_msgFile :: B.ByteString -> IO ()
commit_amend_msgFile path = run_command ("git commit --amend -F \"" <> path <> "\"")

commit_refMsgOnly :: B.ByteString -> IO ()
commit_refMsgOnly ref = run_command ("git commit -C " <> ref <> " --reset-author")

cherrypick :: B.ByteString -> IO ()
cherrypick ref = run_command ("git cherry-pick --allow-empty --allow-empty-message --ff " <> ref)

merge :: Bool -> Bool -> Bool -> [B.ByteString] -> IO ()
merge doCommit ours noff parents = run_command command
  where
    command = "git merge"
                    <> (if doCommit then " --no-edit" else " --no-commit")
                    <> (if ours then " --strategy=ours" else "")
                    <> (if noff then " --no-ff" else "")
                    <> foldl (<>) "" (map (" " <>) parents)

git_resolve_hashes :: [B.ByteString] -> IO [Hash]
git_resolve_hashes refs = do
  mapM_ verify_cmdarg refs
  hashes <- fmap (map (Hash . trim)) $ command_lines ("git rev-parse " <> (mconcat $ map (" " <>) refs)) '\n'
  if length hashes == length refs
    then pure hashes
    else error "Hash number does not match"

verify_cmdarg :: Monad m => B.ByteString -> m ()
verify_cmdarg str = case regex_match str "[\"'\\\\\\(\\)#]|[\001- ]" of
  Just _ -> fail ("Invalid cmdarg: " <> show str)
  Nothing -> pure ()
