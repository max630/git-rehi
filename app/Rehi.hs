{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Rehi where

import Prelude hiding (putStrLn,putStr,writeFile,readFile)

import Data.ByteString(ByteString,uncons)
import Data.ByteString.Char8(putStrLn,putStr,pack,hPutStrLn)
import Data.List(foldl', isPrefixOf)
import Data.Maybe(fromMaybe,isJust,isNothing)
import Data.Monoid((<>))
import Data.Typeable(typeOf)
import Control.Monad(foldM,forM_,when)
import Control.Monad.Catch(displayException,finally,catch,catchJust,catches,SomeException,throwM)
import Control.Monad.Catch(MonadMask,MonadThrow,SomeException(SomeException),Exception,Handler(Handler))
import Control.Monad.Fix(fix)
import Control.Monad.IO.Class(liftIO,MonadIO)
import Control.Monad.Reader(MonadReader,ask)
import Control.Monad.RWS(execRWST, RWST, runRWST)
import Control.Monad.State(put,get,modify',MonadState)
import Control.Monad.Trans(lift)
import Control.Monad.Trans.Reader(ReaderT(runReaderT))
import Control.Monad.Trans.State(evalStateT,execStateT)
import Control.Monad.Trans.Cont(ContT(ContT),evalContT)
import Control.Monad.Trans.Writer(execWriterT)
import Control.Monad.Writer(tell)
import System.Exit (ExitCode(ExitSuccess,ExitFailure),exitWith)
import System.IO(hClose,IOMode(WriteMode,AppendMode),hSetBinaryMode)

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified GHC.IO.Exception as GIE
import qualified Prelude as Prelude
import qualified System.IO as SI

import Rehi.Git.Types (Hash(Hash), hashString)
import Rehi.Utils (equalWith, index_only, readPopen, mapFileLinesM, modifySnd,
                   trim, writeFile, appendToFile, whenM, unlessM, ifM, popen_lines,
                   tryWithRethrowComandFailure,onCommandFailure)
import Rehi.Utils.ArgList(ArgList(ArgList))
import Rehi.Utils.IO(withBinaryFile,readBinaryFile,openBinaryFile,openBinaryTempFile,callCommand,
               createDirectory,copyFile,
               removeDirectoryRecursive,removeFile,doesFileExist,doesDirectoryExist, getArgs,
               lookupEnv, system, initProgram)
import Rehi.Utils.Regex (regex_match, regex_match_with_newlines, regex_match_all, regex_split)

import qualified Rehi.Git.Commands as Cmd

main :: IO ()
main = handleErrors (SI.hPutStrLn SI.stderr) (hPutStrLn SI.stderr) (exitWith . ExitFailure) $ do
  initProgram
  env <- get_env
  flip runReaderT env $ do
    args <- liftIO getArgs
    let parsed = parse_cli args
    case parsed of
      Abort -> abort_rebase
      Continue -> do
        (todo, current, commits, target_ref, marks) <- restore_rebase
        case current of
          Just c -> do
            run_continue c commits
            liftIO (removeFile (envGitDir env `mappend` "/rehi/current"))
          Nothing -> return ()
        lift $ run_rebase (envGitDir env) todo commits target_ref marks Sync
      Skip -> do
        (todo, current, commits, target_ref, marks) <- restore_rebase
        case current of
          Just c -> do
            liftIO $ Cmd.reset $ "HEAD"
            liftIO (removeFile (envGitDir env `mappend` "/rehi/current"))
        lift $ run_rebase (envGitDir env) todo commits target_ref marks Sync
      Current -> do
        let currentPath = envGitDir env `mappend` "/rehi/current"
        liftIO (doesFileExist currentPath) `unlessM` throwM (ExpectedFailure ["No rehi in progress"])
        content <- liftIO $ readBinaryFile currentPath
        liftIO $ putStr ("Current: " <> content <> (if ByteString.null content || BC.last content /= '\n' then "\n" else ""))
      Run dest source_from_arg through source_to_arg target_arg interactive -> do
        git_verify_clean
        initial_branch <- git_get_checkedout_branch
        let
          target_ref = fromMaybe initial_branch target_arg
          source_to = fromMaybe target_ref source_to_arg
        source_from <- case source_from_arg of
          Just s -> pure s
          Nothing | Just _ <- regex_match ".*~1$" dest -> pure dest
          Nothing -> git_merge_base source_to dest
        let
          through' = case regex_match "^(.*)~1$" source_from of
            Just (_ : m : _) -> m : through
            Nothing -> through
        main_run dest source_from through' source_to target_ref initial_branch interactive

data CliMode =
  Abort
  | Continue
  | Skip
  | Current
  | Run { runDest :: ByteString
        , runFrom :: (Maybe ByteString)
        , runThroughs :: [ByteString]
        , runTo :: (Maybe ByteString)
        , runTarget :: (Maybe ByteString)
        , runInteractive :: Bool }
  deriving (Show, Eq)

data Head = Sync | Known Hash deriving Show

data Commits = Commits {
    commitsRefs :: Map.Map ByteString Hash
  , commitsByHash :: Map.Map Hash Entry
  } deriving Show

data Entry = Entry {
    entryAHash :: ByteString
  , entryHash :: Hash
  , entrySubject :: ByteString
  , entryParents :: [Hash]
  , entryTree :: Hash
  , entryBody :: ByteString
  } deriving Show

data Step =
    Pick ByteString
  | Fixup ByteString
  | Edit ByteString
  | Exec ByteString
  | Comment ByteString
  | Merge { mergeRef :: Maybe ByteString, mergeParents :: [ByteString], mergeOurs :: Bool, mergeNoff :: Bool }
  | Mark ByteString
  | Reset ByteString
  | UserComment ByteString
  | TailPickWithComment ByteString ByteString
  deriving (Show, Eq)

data Env a = Env { envGitDir :: ByteString, envRest :: a }

-- Tmp State
data TS = TS {
    tsHead :: Head
  , tsMarks :: Map.Map ByteString Hash
}

-- Tmp Env
type TE = Env Commits

teGitDir = envGitDir

teRefs = commitsRefs . envRest

teByHash = commitsByHash . envRest

pattern TE refs byHash <- Env { envRest = (Commits refs byHash) }

data StepResult = StepPause | StepNext

newtype EditError = EditError ByteString deriving Show

instance Exception EditError

newtype ExpectedFailure = ExpectedFailure [ByteString] deriving Show
instance Exception ExpectedFailure

pattern CommandFailed location <- GIE.IOError { GIE.ioe_type = GIE.OtherError,
                                                GIE.ioe_location = location }

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
    parse_loop interactive (arg0 : arg1 : arg2mb) | length arg2mb == 1 || length arg2mb == 0 && isJust (regex_match "\\.\\." arg1) =
        let
          re_ref0 = "(?:[^\\.]|(?<!\\.)\\.)*"
          re_ref1 = "(?:[^\\.]|(?<!\\.)\\.)+"
          re_sep = "(?<!\\.)\\.\\."
          (source_from, through, source_to) = case regex_match (mconcat ["^(", re_ref0, ")", re_sep, "((?:", re_ref1, re_sep, ")*)(", re_ref0, ")$"]) arg1 of
            Just [all, m1, m2, m3] -> (m1, regex_match_all m2 (mconcat ["(", re_ref1, ")", re_sep]), m3)
            _ -> error ("Invalid source spec:" ++ show arg1)
          arg2 = case arg2mb of
            [] -> Nothing
            [v] -> Just v
          maybeFromString "" = Nothing
          maybeFromString s = Just s
        in Run arg0 (maybeFromString source_from) through (maybeFromString source_to) arg2 interactive
    parse_loop interactive [arg0, arg1] = Run arg0 Nothing [] Nothing (Just arg1) interactive
    parse_loop _ argv = error ("Invalid arguments: " ++ show argv)

main_run :: ByteString -> ByteString -> [ByteString] -> ByteString -> ByteString -> ByteString -> Bool -> ReaderT (Env ()) IO ()
main_run dest source_from through source_to target_ref initial_branch interactive = do
  (todo, commits, dest_hash) <-
    catch
      (init_rebase dest source_from through source_to target_ref initial_branch)
      (\(e :: ExpectedFailure) -> do
        cleanup_save
        throwM e)
  (todo, commits) <- if interactive
    then (do
      let todo' = add_info_to_todo todo commits
      edit_todo todo' commits >>= \case
        Just todo -> pure (todo, commits)
        Nothing -> do
          cleanup_save
          throwM (ExpectedFailure ["Aborted"]))
    else pure (todo, commits)
  if any (\case { UserComment _ -> False ; _ -> True }) todo
    then (do
      gitDir <- askGitDir
      liftIO $ save_todo todo (gitDir <> "/rehi/todo.backup") commits
      liftIO $ Cmd.checkout_detached $ hashString dest_hash
      lift $ run_rebase gitDir todo commits target_ref Map.empty (Known dest_hash))
    else (do
        liftIO(putStrLn "Nothing to do")
        cleanup_save)

restore_rebase = do
  gitDir <- askGitDir
  target_ref <- liftIO (readBinaryFile (gitDir <> "/rehi/target_ref"))
  (commits, marks) <- git_load_commits
  todo <- read_todo (gitDir <> "/rehi/todo") commits
  current <- ifM (liftIO (doesFileExist (gitDir <> "/rehi/current")))
                (do
                  [step] <- read_todo (gitDir <> "/rehi/current") commits
                  pure (Just step))
                (pure Nothing)
  pure (todo, current, commits, target_ref, marks)

init_rebase :: _ -> _ -> _ -> _ -> _ -> _ -> ReaderT (Env a) IO ([_], _, _)
init_rebase dest source_from through source_to target_ref initial_branch = do
  (dest_hash : source_from_hash : source_to_hash : through_hashes ) <-
    liftIO $ Cmd.git_resolve_hashes (dest : source_from : source_to : through)
  init_save target_ref initial_branch
  commits <- git_fetch_cli_commits source_from source_to
  let unknown_parents = find_unknown_parents commits
  commits <- git_fetch_commit_list commits unknown_parents
  case build_rebase_sequence commits source_from_hash source_to_hash through_hashes of
    Right todo -> pure (todo, commits, dest_hash)
    Left msg -> throwM $ ExpectedFailure [msg]

find_unknown_parents commits =
  Set.toList $ Set.fromList [ p | c <- Map.elems (commitsByHash commits),
                                  p <- entryParents c,
                                  not (Map.member p (commitsByHash commits)) ]

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

comments_from_string :: ByteString -> Int -> [Step]
comments_from_string content indent =
  map (\l -> UserComment (mconcat (replicate indent " ") <> l))
      (regex_split content "\\r\\n|\\r|\\n")

add_info_to_todo old_todo commits = old_todo ++ comments_from_string help 0 ++ [UserComment "", UserComment " Commits"] ++ comments
  where
    comments = concatMap (\case
      Pick ah -> from_hash ah
      Fixup ah -> from_hash ah
      Edit ah -> from_hash ah
      Merge (Just ah) _ _ _ -> from_hash ah
      _ -> []) old_todo
    from_hash ah = fromMaybe [] (do
      h <- Map.lookup ah (commitsRefs commits)
      e <- Map.lookup h (commitsByHash commits)
      pure ([UserComment ("----- " <> ah <> " -----")] ++ comments_from_string (entryBody e) 0))

edit_todo old_todo commits = do
  gitDir <- askGitDir
  (todoPath, todoHandle) <- liftIO (openBinaryTempFile (gitDir <> "/rehi") "todo.XXXXXXXX")
  liftIO (hClose todoHandle)
  liftIO $ save_todo old_todo todoPath commits
  retry (do
    -- use git to launch editor to avoid dealing with msys paths in Windows
    liftIO $ Cmd.edit_config_file todoPath
    todo_rc <- read_todo todoPath commits
    verify_marks todo_rc
    pure todo_rc)

verify_marks todo = do
    _ <- foldM (\marks -> \case
                      Mark m | Set.member m marks -> throwM (EditError ("Duplicated mark: " <> m))
                      Mark m -> pure $ Set.insert m marks
                      Pick ref -> check marks ref
                      Fixup ref -> check marks ref
                      Edit ref -> check marks ref
                      Reset ref -> check marks ref
                      Merge _ refs _ _ -> mapM_ (check marks) refs >> pure marks
                      UserComment _ -> pure marks
                      TailPickWithComment _ _ -> pure marks
                      Comment _ -> pure marks
                      Exec _ -> pure marks) Set.empty todo
    pure ()
  where
    check marks (uncons -> Just ((== (ByteString.head "@")) -> True, mark)) | not (Set.member mark marks) = throwM (EditError ("Unknown mark:" <> mark))
    check marks _ = pure marks

run_continue :: (MonadReader (Env a) m, MonadIO m, MonadThrow m) => Step -> t -> m ()
run_continue current commits = do
  liftIO $
    tryWithRethrowComandFailure
      ["callProcess: ", "readCreateProcess: "]
      (ExpectedFailure ["Continue failed - unresolved problems"])
      Cmd.verify_clean
  case current of
    Pick ah -> git_no_uncommitted_changes `unlessM` liftIO (Cmd.commit $ Just ah)
    Merge ahM _ _ _ -> git_no_uncommitted_changes `unlessM` liftIO (Cmd.commit ahM)
    Edit _ -> git_no_uncommitted_changes `unlessM` throwM (ExpectedFailure ["No unstaged changes should be after 'edit'"])
    Fixup _ -> git_no_uncommitted_changes `unlessM` liftIO Cmd.commit_amend
    Exec cmd -> throwM $ ExpectedFailure ["Cannot continue '" <> cmd <> "'", "resolve it manually, then skip or abort"]
    Comment c -> comment c
    _ -> fail ("run_continue: Unexpected " ++ show current)

data FinalizeMode = CleanupData | KeepData

run_rebase gitDir todo commits target_ref marks curHead =
    evalStateT
      (runReaderT doJob (Env gitDir commits))
      (TS curHead marks)
  where
    doJob = do
      result <- mainLoop
      release
      case result of
        CleanupData -> do
          liftIO $ Cmd.checkout_here target_ref
          cleanup_save
        KeepData -> pure ()
    release = do
      (catch :: _ -> (SomeException -> _) -> _)
        sync_head
        (\e -> do
          liftIO $ Prelude.putStrLn ("Fatal error: " <> show e)
          liftIO $ putStrLn "Not possible to continue"
          liftIO $ removeFile (gitDir <> "/rehi/todo"))
    mainLoop = fix (\rec todo -> do
                            case todo of
                              (current : todo) -> do
                                let hasIo = case current of
                                              UserComment _ -> False
                                              TailPickWithComment _ _ -> False
                                              _ -> True
                                when hasIo (do
                                  commits <- envRest <$> ask
                                  liftIO $ save_todo todo (gitDir <> "/rehi/todo") commits
                                  liftIO $ save_todo [current] (gitDir <> "/rehi/current") commits)
                                run_step current >>= \case
                                  StepPause -> pure KeepData
                                  StepNext -> do
                                    when hasIo $ liftIO (removeFile (gitDir <> "/rehi/current"))
                                    rec todo
                              [] -> pure CleanupData) todo

abort_rebase = do
  gitDir <- askGitDir
  initial_branch <- liftIO $ readBinaryFile (gitDir <> "/rehi/initial_branch")
  liftIO $ Cmd.reset initial_branch
  liftIO $ Cmd.checkout_force initial_branch
  cleanup_save

run_step
  :: (MonadIO m,
      MonadReader TE m,
      MonadState TS m) =>
     Step -> m StepResult
run_step rebase_step = do
  evalContT $ do
    case rebase_step of
      Pick ah -> do
        pick =<< resolve_ahash ah
      Edit ah -> do
        commits <- envRest <$> ask
        liftIO $ putStrLn ("Apply: " <> commits_get_subject commits ah)
        pick =<< resolve_ahash ah
        sync_head
        liftIO $ Prelude.putStrLn "Amend the commit and run \"git rehi --continue\""
        returnC $ pure StepPause
      Fixup ah -> do
        commits <- envRest <$> ask
        liftIO $ putStrLn ("Fixup: " <> commits_get_subject commits ah)
        sync_head
        (liftIO . Cmd.fixup) =<< resolve_ahash ah
      Reset ah -> do
        hash_or_ref <- resolve_ahash ah
        fmap (Map.member (Hash hash_or_ref) . teByHash) ask >>= \case
          True -> modify' (\ts -> ts { tsHead = Known $ Hash hash_or_ref})
          False -> do
            liftIO $ Cmd.reset hash_or_ref
            modify' (\ts -> ts{tsHead = Sync})
      Exec cmd -> do
        sync_head
        liftIO
          $ tryWithRethrowComandFailure
              ["callCommand: "]
              (ExpectedFailure
                          [ "Command " <> cmd <> " failed."
                          , "Resolve and run `git rehi --skip` or `git rehi --abort`"])
              (callCommand cmd)
      Comment new_comment -> do
        liftIO $ putStrLn "Updating comment"
        sync_head
        comment new_comment
      Mark mrk -> add_mark mrk
      Merge commentFrom parents ours noff -> merge commentFrom parents ours noff
      UserComment _ -> pure ()
    pure StepNext

add_mark mrk = do
  hashNow <- fmap tsHead get >>= \case
                Known h -> pure h
                Sync -> do
                  [hashNow] <- liftIO $ Cmd.git_resolve_hashes ["HEAD"]
                  pure hashNow
  modify' $ \ts -> ts{ tsMarks = Map.insert mrk hashNow (tsMarks ts) }
  gitDir <- askGitDir
  liftIO $ appendToFile (gitDir <> "/rehi/marks") (mrk <> " " <> hashString hashNow <> "\n")

merge commit_refMb merge_parents_refs ours noff = do
  fmap ((,commit_refMb) . tsHead) get >>= \case
    (Known cachedHash, Just commit_ref) -> do
      (Commits refs byHash) <- envRest <$> ask
      case () of
        _ | Just step_hash <- Map.lookup commit_ref refs
          , Just step_data <- Map.lookup step_hash byHash
          -> fix (\rec actuals expects ->
                    case (actuals, expects) of
                      ("HEAD" : at, eh : et) -> if eh == cachedHash then rec at et else merge_new_
                      (ah : at, eh : et) -> do
                        ahHash <- resolve_ahash ah
                        if ByteString.isPrefixOf ahHash (hashString eh) then rec at et else merge_new_
                      ([], []) -> do
                        liftIO $ putStrLn ("Fast-forwarding unchanged merge: " <> commit_ref <> " " <> entrySubject step_data)
                        modify' (\s -> s{tsHead = Known step_hash})
                      _ -> merge_new_)
                merge_parents_refs (entryParents step_data)
          | otherwise -> merge_new_
    _ -> merge_new_
  where
    merge_new_ = merge_new commit_refMb merge_parents_refs ours noff

merge_new :: (MonadIO m, MonadState TS m, MonadReader TE m) => Maybe ByteString -> [ByteString] -> Bool -> Bool -> m ()
merge_new commit_refMb parents_refs ours noff = do
  [oldHead] <- fmap tsHead get >>= \case
    Known hash -> pure [hash]
    Sync -> liftIO $ Cmd.git_resolve_hashes ["HEAD"]
  sync_head
  liftIO $ putStrLn "Merging"
  parents <- mapM resolve_ahash parents_refs
  let head_pos = index_only "HEAD" parents_refs
  parents <- if head_pos /= 0
              then do
                let
                  (pFirst : pInit, _ : pTail) = splitAt head_pos parents
                liftIO $ Cmd.reset pFirst
                pure (pInit ++ [hashString oldHead] ++ pTail)
              else pure (tail parents)
  liftIO $ tryWithRethrowComandFailure
              ["callProcess: "]
              (ExpectedFailure ["Merge failed. Resolve and --continue or --skip, or --abort"])
              (Cmd.merge (isNothing commit_refMb) ours noff parents)
  case commit_refMb of
    Just commit -> liftIO $
                      tryWithRethrowComandFailure
                        ["callProcess: "]
                        (ExpectedFailure
                          ["Merge commit failed, fastforward?"
                          , "You would probably want to do now one of the following:"
                          , " * force non-fastforward merge"
                          , " * merge some other parent instead of the one from history"
                          , " * skip the step at all (is it a non-interactive rebase?)"
                          , "Then invoke git rehi --skip"])
                        (Cmd.commit_refMsgOnly commit)
    _ -> pure ()

sync_head :: (MonadState TS m, MonadIO m) => m ()
sync_head = do
  fmap tsHead get >>= \case
    Known hash -> do
      liftIO $ Cmd.reset $ hashString hash
      modify' (\t -> t{tsHead = Sync})
    Sync -> pure ()

pick hash = do
  env <- ask
  state <- get
  case tsHead state of
    Known currentHash
      | Just pickData <- Map.lookup (Hash hash) (teByHash env)
      , [pickParent] <- (entryParents pickData)
      , pickParent == currentHash
      -> do
          liftIO $ putStrLn ("Fast-forwarding unchanged commit: " <> entryAHash pickData <> " " <> entrySubject pickData)
          modify' (\s -> s{ tsHead = Known (Hash hash)})
    _ -> do
          sync_head
          liftIO $
              onCommandFailure
                ["callProcess: "]
                (do
                  conflicting_files <-
                    execWriterT (liftIO (popen_lines "git" "status --porcelain -uno" '\n') >>= mapM_ (\case
                                  (regex_match "^[DAU][DAU] (.*)$" -> Just [_, f]) -> tell [f]
                                  _ -> pure ()))
                  throwM $ ExpectedFailure ([ "Conflicting files:" ] ++
                                            (map (" " <>) conflicting_files) ++
                                            [ "Pick `"
                                              <> hash
                                              <> "` failed. Resolve and --continue or --skip, or --abort" ]))
                (Cmd.cherrypick hash)

comment new_comment = do
  gitDir <- askGitDir
  liftIO $ writeFile (gitDir <> "/rehi/commit_msg") new_comment
  liftIO $ Cmd.commit_amend_msgFile (gitDir <> "/rehi/commit_msg")

build_rebase_sequence :: Commits -> Hash -> Hash -> [Hash] -> Either ByteString [Step]
build_rebase_sequence commits source_from_hash source_to_hash through_hashes =
  case find_sequence (commitsByHash commits) source_from_hash source_to_hash through_hashes of
    Right sequence ->
      let
        (marks, _, _)
              = foldl'
                  (\(marks, mark_num, prev_hash) step_hash ->
                    let (marks', mark_num') =
                          foldl'
                            (\v@(marks, mark_num) parent ->
                              case Map.lookup parent marks of
                                Just Nothing ->
                                  (Map.insert parent (Just ("tmp_" <> pack (show mark_num))) marks
                                  , mark_num + 1)
                                _ -> v)
                            (marks, mark_num)
                            (filter (/= prev_hash) $ entryParents (commitsByHash commits Map.! step_hash))
                    in (marks', mark_num', step_hash))
                  (Map.fromList $ zip ([source_from_hash] ++ sequence) (repeat Nothing)
                   , 1 :: Integer
                   , source_from_hash)
                  sequence
        from_mark = maybe [] ((:[]) . Mark) (marks Map.! source_from_hash)
        steps = concat $ zipWith makeStep sequence (source_from_hash : sequence)
        makeStep this prev = reset ++ step ++ maybe [] ((:[]) . Mark) (marks Map.! this)
          where
            thisE = commitsByHash commits Map.! this
            (real_prev, reset) =
              if prev `elem` entryParents thisE
                then (prev, [])
                else case filter (`Map.member` marks) (entryParents thisE) of
                  (h : _) | Just m <- marks Map.! h -> (h, [Reset ("@" <> m)])
                          | Nothing <- marks Map.! h -> error ("Unresolved mark for " <> show h)
                  [] -> error ("No known parents for found step " <> show this)
            step = case entryParents thisE of
              [p] -> [Pick $ entryAHash thisE]
              ps -> make_merge_steps thisE real_prev commits marks
      in Right (from_mark ++ steps)
    Left msg -> Left msg

make_merge_steps thisE real_prev commits marks = singleHead `seq` [Merge (Just ahash) parents ours False]
  where
    parents = map mkParent (entryParents thisE)
    mkParent p | p == real_prev = "HEAD"
               | Just (Just m) <- Map.lookup p marks = "@" <> m
               | Just Nothing <- Map.lookup p marks = error ("Unresolved mark for " <> show p)
               | Just e <- Map.lookup p (commitsByHash commits) = entryAHash e
               | True = error ("Unknown parent: " <> show p)
    singleHead = index_only "HEAD" parents :: Integer
    ahash = entryAHash thisE
    ours = entryTree thisE == entryTree (commitsByHash commits Map.! head (entryParents thisE) )

git_fetch_cli_commits from to = do
  git_fetch_commits ("log -z --ancestry-path --pretty=format:%H:%h:%T:%P:%B" <> [from <> ".." <> to])
                    (Commits Map.empty Map.empty)

git_fetch_commits :: (MonadIO m, MonadMask m, MonadReader (Env a) m) => ArgList -> Commits -> m Commits
git_fetch_commits args commits = do
  gitDir <- askGitDir
  h <- liftIO $ openBinaryFile (gitDir <> "/rehi/commits") (AppendMode)
  liftIO $ hSetBinaryMode h True
  finally
    (do
      execStateT
        ((liftIO $ popen_lines "git" args '\0') >>= mapM (\case
          "\n" -> pure ()
          line -> do
            git_parse_commit_line line
            liftIO $ BC.hPut h line))
        commits)
    (liftIO $ hClose h)

git_load_commits = do
    gitDir <- askGitDir
    commits <- execStateT (mapFileLinesM git_parse_commit_line (gitDir <> "/rehi/commits") '\0') commitsEmpty
    let marksFile = gitDir <> "/rehi/marks"
    marks <- execStateT (liftIO (doesFileExist marksFile) `whenM` mapFileLinesM addMark marksFile '\n') Map.empty
    pure (commits, marks)
  where
    addMark (regex_match "^([0-9a-zA-Z_\\/]+) ([0-9a-fA-F]+)$" -> Just [_, mName, mValue])
                 = modify' (Map.insert mName (Hash mValue))
    addMark line = fail ("Ivalid mark line: " <> show line)

git_parse_commit_line line = do
  case regex_match_with_newlines "^([0-9a-f]+):([0-9a-f]+):([0-9a-f]+):([0-9a-f ]*):(.*)$" line of
    Just [_, Hash -> hash, ahash, Hash -> tree, map Hash . BC.split ' ' -> parents, body] -> do
      verify_hash hash
      mapM_ verify_hash parents
      let
        (subject : _) = BC.split '\n' body
        obj = Entry ahash hash subject parents tree body
      modify' (\c -> c{ commitsByHash = Map.insertWith (const id) hash obj (commitsByHash c)
                      , commitsRefs = Map.insertWith (\hNew hOld -> if hNew == hOld then hOld else error ("Duplicated ref with different hash: " <> show ahash <> "=>" <> show hOld <> ", " <> show hNew))
                                              ahash
                                              hash
                                              (commitsRefs c)})
    _ -> fail ("Could not parse line: " <> show line)

git_merge_base b1 b2 = do
  [base] <- liftIO $ popen_lines "git" ("merge-base -a" <> [b1, b2]) '\n'
  pure base

verify_hash :: Monad m => Hash -> m ()
verify_hash (Hash (regex_match "^[0-9a-f]{40}$" -> Just _)) = pure ()
verify_hash (Hash h) = fail ("Invalid hash: " <> show h)

init_save target_ref initial_branch = do
  gitDir <- askGitDir
  liftIO (doesFileExist (gitDir <> "/rehi")) `whenM` fail "already in progress"
  liftIO $ createDirectory (gitDir <> "/rehi")
  liftIO $ writeFile (gitDir <> "/rehi/target_ref") target_ref
  liftIO $ writeFile (gitDir <> "/rehi/initial_branch") initial_branch

cleanup_save :: (MonadReader (Env a) m, MonadIO m) => m ()
cleanup_save = do
  gitDir <- askGitDir
  liftIO (doesDirectoryExist (gitDir <> "/rehi")) `whenM` (do
    let newBackup = gitDir <> "/rehi/todo.backup"
    liftIO (doesFileExist newBackup) `whenM`
              liftIO (copyFile newBackup (gitDir <> "/rehi_todo.backup"))
    liftIO $ removeDirectoryRecursive (gitDir <> "/rehi"))

commits_get_subject (Commits refs byHash) ah = do
  maybe "???"
        (\h -> maybe "???" entrySubject $ Map.lookup h byHash)
        (Map.lookup ah refs)

save_todo todo path commits = do
  let
    (reverse -> tail, reverse -> main) = span (\case { UserComment _ -> True; TailPickWithComment _ _ -> True; _ -> False }) $ reverse todo
  withBinaryFile path WriteMode $ \out -> do
    forM_ main $ hPutStrLn out . \case
      Pick ah -> "pick " <> ah <> " " <> commits_get_subject commits ah
      Edit ah -> "edit " <> ah <> " " <> commits_get_subject commits ah
      Fixup ah -> "fixup " <> ah <> " " <> commits_get_subject commits ah
      Reset tgt -> "reset " <> tgt
      Exec (regex_match "\\n" -> Just _) -> error "multiline command canot be saved"
      Exec cmd -> "exec " <> cmd
      Comment cmt -> string_from_todo_comment cmt
      Merge ref ps ours noff ->
        ("merge"
          <> (if ours then " --ours" else "")
          <> (if noff then " --no-ff" else "")
          <> maybe "" (" -c " <>) ref
          <> " " <> ByteString.intercalate "," ps
          <> maybe "" ((" " <>) . commits_get_subject commits) ref)
      Mark mrk -> ": " <> mrk
      UserComment cmt -> "# " <> cmt
    if (not $ null tail)
      then do
        hPutStrLn out "end"
        forM_ tail $ hPutStrLn out . \case
          UserComment cmt -> cmt
          TailPickWithComment ah msg
            -> "----- " <> ah <> " -----\n"
                <> string_from_todo_comment msg
      else pure ()

string_from_todo_comment :: ByteString -> ByteString
string_from_todo_comment cmt =
  case regex_match "[^\\n]\\.[$\\n]|[^\\n]$|[^\\n]#" cmt of
    Just _ -> quoted
    Nothing -> "comment\n" <> cmt <> if BC.last cmt == '\n' then "" else "\n" <> ".\n"
  where
    quoted = "comment " <> BC.replicate (BC.length endMark) '{' <> "\n" <> cmt <> endMark <> "\n"
    endMark = fix (\rec p -> if p `ByteString.isInfixOf` cmt then rec (p <> "}") else p) "}}}"

data ReadState = RStCommand | RStDone | RStCommentPlain ByteString | RStCommentQuoted ByteString ByteString deriving Show

read_todo :: (MonadIO m, MonadMask m) => ByteString -> Commits -> m [Step]
read_todo path commits = do
    (s, todo) <- execRWST (mapFileLinesM parseLine path '\n') () RStCommand
    case s of
      RStCommand -> pure todo
      RStDone -> pure todo
      mode -> throwM $ EditError "Unterminated comment"
  where
    parseLine line = do
      get >>= \case
        RStCommand
          | Just [_, cmt] <- regex_match "^#(.*)$" line -> tell [UserComment cmt]
          | Just _ <- regex_match "^end$" line -> put RStDone
          | Just (_ : _ : ah : _) <- regex_match "^(f|fixup) (\\@?[0-9a-zA-Z_\\/]+)( .*)?$" line
              -> tell [Fixup ah]
          | Just (_ : _ : ah : _) <- regex_match "^(p|pick) (\\@?[0-9a-zA-Z_\\/]+)( .*)?$" line
              -> tell [Pick ah]
          | Just (_ : _ : ah : _) <- regex_match "^(e|edit) (\\@?[0-9a-zA-Z_\\/]+)( .*)?$" line
              -> tell [Edit ah]
          | Just (_ : ah : _) <- regex_match "^reset (\\@?[0-9a-zA-Z_\\/]+)$" line
              -> tell [Reset ah]
          | Just (_ : _ : cmd : _) <- regex_match "^(x|exec) (.*)$" line
              -> tell [Exec cmd]
          | Just _ <- regex_match "^comment$" line -> put $ RStCommentPlain ""
          | Just [_, b] <- regex_match "^comment (\\{+)$" line
              -> put $ RStCommentQuoted "" (BC.length b `BC.replicate` '}')
          | Just [_, options, _, parents] <- regex_match "^merge(( --ours| --no-ff| -c \\@?[0-9a-zA-Z_\\/]+)*) ([^ ]+)" line
              -> do
                merge <- fix (\rec m l -> if
                                  | ByteString.null l -> pure m
                                  | Just [_, rest] <- regex_match "^ --ours( .*)?$" l -> rec m{ mergeOurs = True } rest
                                  | Just [_, rest] <- regex_match "^ --no-ff( .*)?$" l -> rec m{ mergeNoff = True } rest
                                  | Just [_, ref, rest] <- regex_match "^ -c (\\@?[0-9a-zA-Z_\\/]+)( .*)?$" l -> rec m{mergeRef = Just ref} rest
                                  | otherwise -> throwM $ EditError ("Unexpected merge options: " <> l))
                              (Merge Nothing (BC.split ',' parents) False False)
                              options
                tell [merge]
          | Just [_, mrk] <- regex_match "^: (.*)$" line
              -> maybe (tell [Mark mrk])
                  (const $ throwM (EditError ("Dangerous symbols in mark name: " <> mrk)))
                  (regex_match "[^0-9a-zA-Z_]" mrk)
          | Just _ <- regex_match "^[ \\t]*$" line -> pure ()
        RStCommentPlain cmt0
          | Just [_, cmt] <- regex_match "^# (.*)$" line -> tell [UserComment cmt]
          | line == "." -> tell [Comment cmt0] >> put RStCommand
          | otherwise -> put $ RStCommentPlain (cmt0 <> line <> "\n")
        RStCommentQuoted cmt0 quote
          | quote `ByteString.isSuffixOf` line -> tell [Comment (cmt0 <> ByteString.take (ByteString.length line - ByteString.length quote) line)] >> put RStCommand
          | otherwise -> put $ RStCommentQuoted (cmt0 <> line <> "\n") quote
        RStDone -> tell [UserComment line]
        mode -> throwM $ EditError ("Unexpected line in mode " <> BC.pack (show mode) <> ": " <> line)

commitsEmpty = Commits Map.empty Map.empty

returnC x = ContT $ const x

data FsThreadState = FsReady | FsFinalizeMergebases | FsWaitChildren | FsDone deriving Eq

data FsThread = FsThread { fsstState :: FsThreadState, fsstCurrent :: Hash, fsstTodo :: [Hash] }

data FsWaiter = FsWaiter { fswThread :: Int, fswLeft :: Int, fswTodo :: Set.Set Hash }

data FS = FS {
                         fssThreads :: Map.Map Int FsThread,
                         fssSchedule :: [Int],
                         fssNextThreadId :: Int,
                         fssChildrenWaiters :: Map.Map Hash FsWaiter,
                         fssTerminatingCommits :: Set.Set Hash }

find_sequence :: Map.Map Hash Entry -> Hash -> Hash -> [Hash] -> Either ByteString [Hash]
find_sequence commits from to through =
  step (FS (Map.singleton 1 (FsThread FsReady to [])) [1] 2 Map.empty Set.empty)
  where
    children_num = Map.unionsWith (+)
                        ((Map.fromList $ map (,0) (from : to : Map.keys commits))
                        : map (Map.fromList . map (,1) . entryParents) (Map.elems commits))
    step = \case
      FS { fssSchedule = [] } -> Left "No path found"
      s@(FS ts sc@(n : _) nextId childerWaiters terminatingCommits)
        | FsDone <- fsstState (ts Map.! n) -> Right $ reverse $ fsstTodo (ts Map.! n)
        | otherwise -> case break ((`elem` ([FsReady, FsFinalizeMergebases] :: [FsThreadState])) . fsstState . (ts Map.!)) sc of
            (_, []) -> error "No thread is READY"
            (scH, (scC@((ts Map.!) -> FsThread curState curHash curTodo) : scT))
              | Set.member curHash terminatingCommits -> step s{ fssSchedule = scH ++ scT }
              | curState == FsFinalizeMergebases ->
                let
                  ts' = if children_num Map.! curHash == 1
                          then ts
                          else case Map.lookup curHash childerWaiters of
                            Nothing -> ts
                            Just (FsWaiter { fswThread = waiter }) ->
                              Map.adjust (\ws -> ws { fsstState = FsFinalizeMergebases }) waiter ts
                  (new_tasks, nextId') = makeParentTasks nextId
                in step (FS (Map.union (Map.fromList new_tasks) ts')
                            (scH ++ map fst new_tasks ++ scT)
                            nextId'
                            childerWaiters
                            (Set.insert curHash terminatingCommits))
              | curHash == from ->
                let
                  ts' = Map.adjust (\t -> t { fsstState = FsDone }) scC ts
                  keepCurrent = all (`Set.member` todoSet) through
                  (new_tasks, nextId') = makeParentTasks nextId
                in step s { fssThreads = Map.union (Map.fromList new_tasks) ts',
                            fssSchedule = scH ++ (if keepCurrent then [scC] else []) ++ map fst new_tasks ++ scT,
                            fssNextThreadId = nextId' }
              | children_num Map.! curHash > 1 && not (Map.member curHash childerWaiters) ->
                step s { fssThreads = Map.adjust (\t -> t { fsstState = FsWaitChildren }) scC ts,
                         fssChildrenWaiters = Map.insert curHash
                                                         (FsWaiter scC ((children_num Map.! curHash) - 1) todoSet)
                                                         childerWaiters }
              | children_num Map.! curHash > 1, Just waiter <- Map.lookup curHash childerWaiters, fswLeft waiter > 0 ->
                let
                  (todo', todoIdx') = foldl' (\(t, i) h -> if Set.member h i then (t,i) else (t ++ [h], Set.insert h i))
                                             (fsstTodo (ts Map.! (fswThread waiter)), fswTodo waiter)
                                             curTodo
                  left' = fswLeft waiter - 1
                in step s{ fssThreads = Map.adjust (\t -> t{fsstTodo = todo',
                                                            fsstState = if left' == 0 then FsReady else fsstState t})
                                                   (fswThread waiter)
                                                   ts,
                           fssChildrenWaiters = Map.adjust (\w -> w{ fswLeft = left', fswTodo = todoIdx' }) curHash childerWaiters,
                           fssSchedule = scH ++ scT }
              | otherwise ->
                let
                  curTodo' = curTodo ++ [curHash]
                  (newTasks, nextId') = makeParentTasksEx (\p -> FsThread FsReady p curTodo') nextId
                in step s{ fssThreads = Map.union (Map.fromList newTasks) ts,
                           fssSchedule = scH ++ map fst newTasks ++ scT,
                           fssNextThreadId = nextId' }
              where
                todoSet = Set.fromList curTodo
                makeParentTasksEx newThread fromId =
                  let tasks = zip [fromId ..] $ map newThread
                                              $ maybe [] entryParents $ Map.lookup curHash commits
                      id = last (fromId : map ((+ 1) . fst) tasks)
                  in (tasks, id)
                makeParentTasks = makeParentTasksEx (\p -> FsThread FsFinalizeMergebases p [])

resolve_ahash :: (MonadReader TE m, MonadState TS m) => ByteString -> m ByteString
resolve_ahash ah = do
  refs <- fmap teRefs ask
  case regex_match "^@(.*)$" ah of
    Just [_,mrk] -> do
      marks <- fmap tsMarks get
      pure $ maybe (error ("Mark " <> show mrk<> " not found")) hashString (Map.lookup mrk marks)
    Nothing -> pure $ maybe ah hashString (Map.lookup ah refs)

git_no_uncommitted_changes :: MonadIO m => m Bool
git_no_uncommitted_changes = liftIO (system "git diff-index --quiet --ignore-submodules HEAD") >>= \case
  ExitSuccess -> pure True
  _ -> pure False

retry :: (MonadMask m, MonadIO m) => m x -> m (Maybe x)
retry func = fix $ \rec -> do
  res <- catch
          (func >>= (pure . Right))
          (\(EditError msg) -> pure $ Left msg)
  case res of
    Right x -> pure (Just x)
    Left msg -> do
      liftIO $ putStrLn ("Error: " <> msg)
      liftIO $ putStrLn "Retry (y/N)?"
      answer <- liftIO
        $ catchJust
            (\case
                (GIE.IOError { GIE.ioe_type = GIE.EOF })
                  -> Just "n"
                _ -> Nothing)
            ByteString.getLine
            pure
      if "y" `ByteString.isPrefixOf` answer || "Y" `ByteString.isPrefixOf` answer
        then rec
        else pure Nothing

git_fetch_commit_list commits [] = pure commits
git_fetch_commit_list commits unknowns = do
  let
    (map hashString -> us, usRest) = Prelude.splitAt 20 unknowns
  commits <- git_fetch_commits
    ("show -z --no-patch --pretty=format:%H:%h:%T:%P:%B" <> ArgList us)
    commits
  git_fetch_commit_list commits usRest

get_env = do
  gitDir <- readPopen "git rev-parse --git-dir"
  pure $ Env gitDir ()

git_verify_clean = do
  git_no_uncommitted_changes `unlessM` fail "Not clean working directory"
  gitDir <- askGitDir
  liftIO (doesFileExist (gitDir <> "/rebase-apply")) `whenM` fail "git-am or rebase in progress"
  liftIO (doesFileExist (gitDir <> "/rebase-merge")) `whenM` fail "rebase in progress"

git_get_checkedout_branch = do
  head_path <- liftIO $ readPopen "git symbolic-ref -q HEAD"
  case regex_match "^refs/heads/(.*)" head_path of
    Just [_, p] -> pure p
    _ -> fail ("Unsupported ref checked-out: " ++ show head_path)

askGitDir :: MonadReader (Env a) m => m ByteString
askGitDir = ask >>= \r -> pure (envGitDir r)

handleErrors :: (String -> IO ()) -> (ByteString -> IO ()) -> (Int -> IO a) -> IO a -> IO a
handleErrors printCb printBSCb exitCb action =
  action `catches` ([Handler catchExpected, Handler catchIO, Handler catchAll] :: [Handler IO _])
  where
    catchExpected (ExpectedFailure msg) = do
      mapM_ printBSCb msg
      exitCb 1
    catchAll (SomeException e) = do
      printCb ("Internal error: " ++ show (typeOf e))
      printCb ("Message: " ++ displayException e)
      exitCb 1
    catchIO (e :: GIE.IOException)
      | GIE.UserError <- GIE.ioe_type e = do
      printCb ("Unexpected happened: " ++ GIE.ioe_description e)
      exitCb 1
      | otherwise = do
      printCb ("IO error: " ++ displayException e)
      exitCb 1
