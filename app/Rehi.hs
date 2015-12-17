{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Rehi where

import Prelude hiding (putStrLn)

import Data.ByteString(ByteString,putStrLn)
import Data.List(find)
import Data.Maybe(fromMaybe,isJust)
import Data.Monoid((<>))
import Control.Monad(liftM)
import Control.Monad.Fix(fix)
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Trans.Except(ExceptT)
import Control.Monad.Trans.Reader(ReaderT(runReaderT),ask)
import System.IO(hClose)
import System.Posix.ByteString(RawFilePath,removeLink,fileExist)
import System.Posix.Env.ByteString(getArgs)
import System.Posix.Temp.ByteString(mkstemp)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

main :: IO ()
main = do
  env <- get_env
  flip runReaderT env $ do
    args <- liftIO getArgs
    let parsed = parse_cli args
    case parsed of
      Abort -> abort_rebase
      Continue -> do
        (todo, current, commits, target_ref) <- restore_rebase
        case current of
          Just c -> do
            run_continue c commits
            liftIO (removeLink (envGitDir env `mappend` "/rehi/current"))
          Nothing -> return ()
        let commits' = commits { stateHead = Sync }
        run_rebase todo commits' target_ref
      Skip -> do
        (todo, current, commits, target_ref) <- restore_rebase
        case current of
          Just c -> do
            liftIO (run_command "git rest --hard HEAD")
            liftIO (removeLink (envGitDir env `mappend` "/rehi/current"))
      Current -> do
        let currentPath = envGitDir env `mappend` "/rehi/current"
        liftIO (fileExist currentPath) >>= \case
          True -> do
            content <- liftIO $ read_file currentPath
            liftIO $ putStrLn ("Current: " `mappend` content)
          False -> error "No rehi in progress"
      Run dest source_from_arg through source_to_arg target_arg interactive -> do
        git_verify_clean
        initial_branch <- git_get_checkedout_branch
        let
          target_ref = fromMaybe initial_branch target_arg
          source_to = fromMaybe target_ref source_to_arg
        source_from <- case source_from_arg of
          Just s -> pure s
          Nothing | Just _ <- regex_match dest ".*~1$" -> pure dest
          Nothing -> git_merge_base source_to dest
        let
          through' = case regex_match source_from "^(.*)~1$" of
            Just (_ : m : _) -> m : through
            Nothing -> through
        main_run dest source_from through' source_to target_ref initial_branch interactive

data CliMode =
  Abort
  | Continue
  | Skip
  | Current
  | Run ByteString (Maybe ByteString) [ByteString] (Maybe ByteString) (Maybe ByteString) Bool

newtype Hash = Hash { hashString :: ByteString } deriving (Eq, Ord, Show)

data Head = Sync | Known Hash

data Commits = Commits {
    stateHead :: Head
  , stateRefs :: Map.Map ByteString Hash
  , stateMarks :: Map.Map ByteString Hash
  , stateByHash :: Map.Map Hash Entry
  }

data Entry = Entry {
    entryAHash :: ByteString
  , entryHash :: Hash
  , entrySubject :: ByteString
  , entryParents :: [Hash]
  , entryTree :: Hash
  }

data Step =
    Pick ByteString
  | Fixup ByteString
  | Edit ByteString
  | Exec ByteString
  | Comment ByteString
  | Merge (Maybe ByteString) [ByteString] Bool Bool
  | Mark ByteString
  | Reset ByteString
  | UserComment ByteString
  | TailPickWithComment ByteString ByteString

data Env = Env { envGitDir :: RawFilePath }

newtype EditError = EditError ByteString

parse_cli = parse_loop False
  where
    parse_loop _ ("-i" : argv') = parse_loop True argv'
    parse_loop _ ("--interactive" : argv') = parse_loop True argv'
    parse_loop _ argv@("--abort" : _ : _ ) = error ("Extra argument:" ++ show argv)
    parse_loop _ ["--abort"] = Abort
    parse_loop _ argv@("--continue" : _ : _ ) = error ("Extra argument:" ++ show argv)
    parse_loop _ ["--continue"] = Continue
    parse_loop _ argv@("--skip" : _ : _ ) = error ("Extra argument:" ++ show argv)
    parse_loop _ ["--skip"] = Skip
    parse_loop _ argv@("--current" : _ : _ ) = error ("Extra argument:" ++ show argv)
    parse_loop _ ["--current"] = Current
    parse_loop interactive [dest] = Run dest Nothing [] Nothing Nothing interactive
    parse_loop interactive (arg0 : arg1 : arg2mb) | length arg2mb == 1 || length arg2mb == 0 && isJust (regex_match arg1 "\\.\\.") =
        let
          re_ref0 = "(?:[^\\.]|(?<!\\.)\\.)*"
          re_ref1 = "(?:[^\\.]|(?<!\\.)\\.)+"
          re_sep = "(?<!\\.)\\.\\."
          (source_from, through, source_to) = case regex_match arg1 (mconcat ["(", re_ref0, ")", re_sep, "((?:", re_ref1, re_sep, ")*)(", re_ref0, ")^$"]) of
            Just [all, m1, m2, m3] -> (m1, regex_match_all m2 (mconcat ["(", re_ref1, ")", re_sep]), m3)
            _ -> error ("Invalid source spec:" ++ show arg1)
          arg2 = case arg2mb of
            [] -> Nothing
            [v] -> Just v
        in Run arg0 (Just source_from) through (Just source_to) arg2 interactive
    parse_loop interactive [arg0, arg1] = Run arg0 Nothing [] Nothing (Just arg1) interactive
    parse_loop _ argv = error ("Invalid arguments: " ++ show argv)
  
main_run dest source_from through source_to target_ref initial_branch interactive = do
  (todo, commits, dest_hash) <- init_rebase dest source_from through source_to target_ref initial_branch
  (todo, commits) <- if interactive
    then (do
      let todo = add_info_to_todo todo commits
      edit_todo todo commits >>= \case
        Just tc -> pure tc
        Nothing -> do
          cleanup_save
          fail "Aborted")
    else pure (todo, commits)
  if any (\case { UserComment _ -> False ; _ -> True }) todo
    then (do
      let commits = commits{ stateHead = Known dest_hash }
      gitDir <- askGitDir
      save_todo todo (gitDir <> "/rehi/todo.backup") commits
      liftIO (run_command ("git checkout --quiet --detach " <> hashString dest_hash))
      run_rebase todo commits target_ref)
    else (do
        liftIO(putStrLn "Nothing to do")
        cleanup_save)

restore_rebase = do
  gitDir <- askGitDir
  target_ref <- liftIO (read_file (gitDir <> "/rehi/target_ref"))
  commits <- git_load_commits
  todo <- read_todo (gitDir <> "/rehi/todo") commits
  current <- liftIO (fileExist (gitDir <> "/rehi/current") >>= \case
    True -> do
      [step] <- read_todo (gitDir <> "/rehi/current") commits
      pure (Just step)
    False -> pure Nothing)
  pure (todo, current, commits, target_ref)

init_rebase :: _ -> _ -> _ -> _ -> _ -> _ -> ReaderT Env IO ([_], _, _)
init_rebase dest source_from through source_to target_ref initial_branch = do
  (dest_hash : source_from_hash : source_to_hash : through_hashes ) <- git_resolve_hashes (dest : source_from : source_to : through)
  init_save target_ref initial_branch
  commits <- git_fetch_cli_commits source_from source_to
  let unknown_parents = find_unknown_parents commits
  commits <- git_fetch_commit_list commits unknown_parents
  let todo = build_rebase_sequence commits source_from_hash source_to_hash through_hashes
  pure (todo, commits, dest_hash)

find_unknown_parents commits =
  Set.toList $ Set.fromList [ p | c <- Map.elems (stateByHash commits),
                                  p <- entryParents c,
                                  not (Map.member p (stateByHash commits)) ]

help = "Commands:\n\
       \\n\
       \ pick\n\
       \ fixup\n\
       \ edit\n\
       \ exec\n\
       \ comment\n\
       \ merge\n\
       \ :\n\
       \ reset\n\
       \ end\n"

comments_from_string content indent = map (\l -> UserComment (mconcat (replicate indent " ") <> l)) (regex_split content "\\r\\n|\\r|\\n")

add_info_to_todo old_todo commits = old_todo ++ comments_from_string help 0 ++ [UserComment "", UserComment " Commits"] ++ comments
  where
    comments = concatMap (\case
      Pick ah -> from_hash ah
      Fixup ah -> from_hash ah
      Edit ah -> from_hash ah
      Merge (Just ah) _ _ _ -> from_hash ah
      _ -> []) old_todo
    from_hash ah = fromMaybe [] (do
      h <- Map.lookup ah (stateRefs commits)
      body <- Map.lookup h (stateByHash commits)
      pure ([UserComment ("----- " <> ah <> " -----")] ++ comments_from_string body 0))

edit_todo old_todo commits = do
  gitDir <- askGitDir
  (todoPath, todoHandle) <- liftIO (mkstemp (gitDir <> "/rehi/todo.XXXXXXXX"))
  liftIO (hClose todoHandle)
  save_todo old_todo todoPath commits
  editor <- git_sequence_editor
  retry (do
    liftIO (run_command editor (" " <> todoPath))
    todo_rc <- read_todo_check todoPath commits
    verify_marks todo_rc
    pure todo_rc)

git_sequence_editor = undefined

retry :: ExceptT EditError _m _x -> _m _x
retry = undefined

build_rebase_sequence = undefined

git_resolve_hashes = undefined

init_save = undefined

git_fetch_cli_commits = undefined

git_fetch_commit_list = undefined

get_env = undefined

abort_rebase = undefined

run_continue = undefined

save_todo = undefined

cleanup_save = undefined

read_file = undefined

read_todo = undefined

read_todo_check = undefined

verify_marks = undefined

git_verify_clean = undefined

git_get_checkedout_branch = undefined

git_merge_base = undefined

git_load_commits = undefined

regex_match :: ByteString -> ByteString -> Maybe [ByteString]
regex_match = undefined

regex_match_all = undefined

regex_split = undefined

run_rebase = undefined

run_command = undefined

askGitDir :: Monad m => ReaderT Env m RawFilePath
askGitDir = ask >>= \r -> pure (envGitDir r)
