{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Rehi where

import Prelude hiding (putStrLn,writeFile)

import Data.ByteString(ByteString,uncons)
import Data.ByteString.Char8(putStrLn,pack)
import Data.Foldable(toList,find)
import Data.List(foldl')
import Data.Maybe(fromMaybe,isJust,maybe)
import Data.Monoid((<>))
import Control.Monad(liftM,foldM,mapM_)
import Control.Monad.Catch(finally,catch,SomeException)
import Control.Monad.Fix(fix)
import Control.Monad.IO.Class(liftIO,MonadIO)
import Control.Monad.Reader(MonadReader,ask)
import Control.Monad.State(put,get,modify',MonadState)
import Control.Monad.Trans.Except(ExceptT,runExceptT,throwE)
import Control.Monad.Trans.Reader(ReaderT(runReaderT))
import Control.Monad.Trans.State(StateT,evalStateT,execStateT)
import Control.Monad.Trans.Cont(ContT(ContT),evalContT)
import System.IO(hClose)
import System.Posix.ByteString(RawFilePath,removeLink,fileExist)
import System.Posix.Env.ByteString(getArgs)
import System.Posix.Temp.ByteString(mkstemp)
import System.Posix.Types(Fd)
import System.Posix.Files(unionFileModes,ownerReadMode,ownerWriteMode)

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Prelude as Prelude
import qualified "unix-bytestring" System.Posix.IO.ByteString as UB
import qualified "unix" System.Posix.IO.ByteString as U

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
  deriving Show

data Env = Env { envGitDir :: RawFilePath }

data StepResult = StepPause | StepNext

newtype EditError = EditError ByteString deriving Show

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
        Just todo -> pure (todo, commits)
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
  todo <- fromExcept $ read_todo (gitDir <> "/rehi/todo") commits
  current <- liftIO (fileExist (gitDir <> "/rehi/current") >>= \case
    True -> do
      [step] <- fromExcept $ read_todo (gitDir <> "/rehi/current") commits
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
    liftIO (run_command (editor <> " " <> todoPath))
    todo_rc <- read_todo todoPath commits
    verify_marks todo_rc
    pure todo_rc)

verify_marks todo = do
    foldM (\marks -> \case
                      Mark m | Set.member m marks -> throwE (EditError ("Duplicated mark: " <> m))
                      Mark m -> pure $ Set.insert m marks
                      Pick ref -> check marks ref
                      Fixup ref -> check marks ref
                      Edit ref -> check marks ref
                      Reset ref -> check marks ref
                      Merge _ refs _ _ -> mapM_ (check marks) refs >> pure marks) Set.empty todo
    pure ()
  where
    check marks (uncons -> Just ((== (ByteString.head "@")) -> True, mark)) | not (Set.member mark marks) = throwE (EditError ("Unknown mark:" <> mark))
    check marks _ = pure marks

run_continue current commits = do
  liftIO $ run_command ("git rev-parse --verify HEAD >/dev/null"
                          <> " && git update-index --ignore-submodules --refresh"
                          <> " && git diff-files --quiet --ignore-submodules")
  case current of
    Pick ah -> git_no_uncommitted_changes >>= \case
      True -> pure ()
      False -> liftIO $ run_command ("git commit -c " <> ah)
    Merge ahM _ _ _ -> git_no_uncommitted_changes >>= \case
      True -> pure ()
      False -> liftIO $ run_command ("git commit " <> maybe "" ("-c" <>) ahM)
    Edit _ -> git_no_uncommitted_changes >>= \case
      True -> pure ()
      False -> fail "No unstaged changes should be after 'edit'"
    Fixup _ -> git_no_uncommitted_changes >>= \case
      True -> pure ()
      False -> liftIO $ run_command "git commit --amend"
    Exec cmd -> fail ("Cannot continue '" ++ show cmd ++ "'; resolve it manually, then skip or abort")
    Comment c -> comment c
    _ -> fail ("run_continue: Unexpected " ++ show current)

-- TODO mutable commits
run_rebase todo commits target_ref = do
    evalStateT (finally doJob release) (todo, commits)
    liftIO $ run_command ("git checkout -B " <> target_ref)
    cleanup_save
  where
    release = do
      (catch :: _ -> (SomeException -> _) -> _)
        sync_head
        (\e -> do
          liftIO $ Prelude.putStrLn ("Fatal error: " <> show e)
          liftIO $ putStrLn "Not possible to continue"
          gitDir <- askGitDir
          liftIO $ removeLink (gitDir <> "/rehi/todo"))
    doJob = fix $ \rec -> do
                            (todo, commits) <- get
                            case todo of
                              (current : todo) -> do
                                gitDir <- askGitDir
                                save_todo todo (gitDir <> "/rehi/todo") commits
                                save_todo current (gitDir <> "/rehi/current") commits
                                put (todo, commits)
                                run_step current >>= \case
                                  StepPause -> pure ()
                                  StepNext -> do
                                    liftIO (removeLink (gitDir <> "/rehi/current"))
                                    rec
                              [] -> pure ()

abort_rebase = do
  gitDir <- askGitDir
  initial_branch <- liftIO $ read_file (gitDir <> "/rehi/initial_branch")
  liftIO $ run_command ("git reset --hard " <> initial_branch)
  liftIO $ run_command ("git checkout -f " <> initial_branch)
  cleanup_save

run_step rebase_step = do
  commits <- fmap snd get
  evalContT $ do
    case rebase_step of
      Pick ah -> do
        pick $ resolve_ahash ah commits
      Edit ah -> do
        liftIO $ putStrLn ("Apply: " <> commits_get_subject commits ah)
        pick $ resolve_ahash ah commits
        sync_head
        liftIO $ Prelude.putStrLn "Amend the commit and run \"git rehi --continue\""
        returnC $ pure StepPause
      Fixup ah -> do
        liftIO $ putStrLn ("Fixup: " <> commits_get_subject commits ah)
        sync_head
        liftIO $ run_command ("git cherry-pick --allow-empty --allow-empty-message --no-commit " <> resolve_ahash ah commits
                                <> " && git commit --amend --reset-author --no-edit")
      Reset ah -> do
        let hash_or_ref = resolve_ahash ah commits
        case Map.lookup hash_or_ref (stateRefs commits) of
          Just _ -> modify' (modifySnd (\c -> c{stateHead = Known hash_or_ref}))
          Nothing -> do
            liftIO $ run_command ("git reset --hard " <> hash_or_ref)
            modify' (modifySnd (\c -> c{stateHead = Sync}))
      Exec cmd -> do
        sync_head
        liftIO $ run_command cmd
      Comment new_comment -> do
        liftIO $ putStrLn "Updating comment"
        sync_head
        comment new_comment
      Mark mrk -> do
        hashNow <- fmap (stateHead . snd) get >>= \case
                      Known h -> pure h
                      Sync -> do
                        [hashNow] <- git_resolve_hashes ["HEAD"]
                        pure hashNow
        modify' $ modifySnd $ \c -> c{ stateMarks = Map.insert mrk hashNow (stateMarks c)}
        gitDir <- askGitDir
        liftIO $ appendToFile (gitDir <> "/rehi/current") (mrk <> " " <> hashString hashNow <> "\n")
      Merge commentFrom parents ours noff -> merge commentFrom parents ours noff
      UserComment _ -> pure ()
    pure StepNext

merge commit_refMb merge_parents_refs ours noff = do
  commits <- fmap snd get
  case (stateHead commits, commit_refMb) of
    (Known cachedHash, Just commit_ref)
      | Just step_hash <- Map.lookup commit_ref (stateRefs commits)
      , Just step_data <- Map.lookup step_hash (stateByHash commits)
      , equalWith (\expect_ref actual_hash
                      -> case expect_ref of
                          "HEAD" -> cachedHash == actual_hash
                          _ -> (resolve_ahash expect_ref commits) `ByteString.isPrefixOf` hashString actual_hash) -- FIXME: sometimes expected parent is unknown so need to do prefix compare here
                  merge_parents_refs (entryParents step_data)
      -> do
          liftIO $ putStrLn ("Fast-forwarding unchanged merge: " <> commit_ref <> " " <> entrySubject step_data)
          modify' (modifySnd (\c -> c{stateHead = Known step_hash}))
    _ -> merge_new commit_refMb merge_parents_refs ours noff

equalWith f [] [] = True
equalWith f (x : xs) (y : ys) = if f x y then equalWith f xs ys else False
equalWith _ _ _ = False

merge_new commit_refMb parents_refs ours noff = do
  sync_head
  liftIO $ putStrLn "Merging"
  let
    commandHead = "git merge"
                    <> maybe " --no-edit" (const " --no-commit") commit_refMb
                    <> (if ours then " --strategy=ours" else "")
                    <> (if noff then " --no-ff" else "") :: ByteString
    parents = map resolve_ahash parents_refs
    head_pos = index_only "HEAD" parents_refs
  parents <- if head_pos /= 0
              then do
                liftIO $ run_command ("git reset --hard " <> head parents)
                let
                  (pFirst : pInit, _ : pTail) = splitAt head_pos parents
                pure (pInit ++ [pFirst] ++ pTail)
              else pure (tail parents)
  let command = commandHead <> foldl (<>) "" (map (" " <>) parents)
  liftIO $ run_command command
  case commit_refMb of
    Just commit -> liftIO $ run_command ("git commit -C " <> commit <> " --reset-author")
    _ -> pure ()

index_only x ys = fromMaybe (error "index_only: not found") (foldl' step Nothing $ zip [0 .. ] ys)
  where
    step prev (n, y) | x == y = case prev of { Nothing -> Just n; Just _ -> error "index_only: duplicate" }
    step prev _ = prev

sync_head :: (MonadState ([Step], Commits) m, MonadIO m) => m ()
sync_head = do
  fmap (stateHead . snd) get >>= \case
    Known hash -> do
      liftIO $ run_command ("git reset --hash " <> hashString hash)
      modify' (modifySnd (\c -> c{stateHead = Sync}))
    Sync -> pure ()

pick hash = do
  commits <- fmap snd get
  case stateHead commits of
    Known currentHash
      | Just pickData <- Map.lookup (Hash hash) (stateByHash commits)
      , [pickParent] <- (entryParents pickData)
      , pickParent == currentHash
      -> do
          liftIO $ putStrLn ("Fast-forwarding unchanged commit: " <> entryAHash pickData <> " " <> entrySubject pickData)
          modify' (modifySnd (\c -> c{stateHead = Known (Hash hash)}))
    _ -> do
          sync_head
          liftIO $ run_command ("git cherry-pick --allow-empty --allow-empty-message --ff " <> hash)

comment new_comment = do
  gitDir <- askGitDir
  liftIO $ writeFile (gitDir <> "/rehi/commit_msg") new_comment
  liftIO $ run_command ("git commit --amend -F \"" <> gitDir <> "/rehi/commit_msg\"")

build_rebase_sequence :: Commits -> Hash -> Hash -> [Hash] -> [Step]
build_rebase_sequence commits source_from_hash source_to_hash through_hashes = from_mark ++ steps
  where
    sequence = find_sequence (stateByHash commits) source_from_hash source_to_hash through_hashes
    (marks, _, _)
          = foldl'
              (\(marks, mark_num, prev_hash) step_hash ->
                let (marks, mark_num) =
                      foldl'
                        (\v@(marks, mark_num) parent ->
                          case Map.lookup parent marks of
                            Just Nothing ->
                              (Map.insert parent (Just ("tmp_" <> pack (show mark_num))) marks
                              , mark_num + 1)
                            _ -> v)
                        (marks, mark_num)
                        (entryParents (stateByHash commits Map.! step_hash))
                in (marks, mark_num, step_hash))
              (Map.fromList $ zip ([source_from_hash] ++ sequence) (repeat Nothing)
               , 1
               , source_from_hash)
              sequence
    from_mark = map (Mark . fromMaybe "build_rebase_sequence: unknown mark for from")
                    (toList $ Map.lookup source_from_hash marks)
    steps = concat $ zipWith makeStep sequence (source_from_hash : sequence)
    makeStep this prev = reset ++ step
      where
        thisE = stateByHash commits Map.! this
        (real_prev, reset) =
          if prev `elem` entryParents thisE
            then (prev, [])
            else case filter (`Map.member` marks) (entryParents thisE) of
              (h : _) | Just m <- marks Map.! h -> (h, [Reset m])
                      | Nothing <- marks Map.! h -> error ("Unresolved mark for " <> show h)
              [] -> error ("No known parents for found step " <> show this)
        step = case entryParents thisE of
          [p] -> [Pick $ entryAHash thisE]
          ps -> make_merge_steps thisE real_prev commits marks

make_merge_steps thisE real_prev commits marks = singleHead `seq` [Merge (Just ahash) parents ours False]
  where
    parents = map mkParent (entryParents thisE)
    mkParent p | p == real_prev = "HEAD"
               | Just (Just m) <- Map.lookup p marks = "@" <> m
               | Just Nothing <- Map.lookup p marks = error ("Unresolved mark for " <> show p)
               | Just e <- Map.lookup p (stateByHash commits) = entryAHash e
               | True = error ("Unknown parent: " <> show p)
    singleHead = index_only "HEAD" parents
    ahash = entryAHash thisE
    ours = entryTree thisE == entryTree (stateByHash commits Map.! head (entryParents thisE) )

git_resolve_hashes :: MonadIO m => [ByteString] -> m [Hash]
git_resolve_hashes refs = do
  mapM_ verify_cmdarg refs
  hashes <- fmap (map (Hash . trim)) $ liftIO $ command_lines ("git rev-parse " <> (mconcat $ map (" " <>) refs))
  if length hashes == length refs
    then pure hashes
    else error "Hash number does not match"

git_fetch_cli_commits from to = do
  verify_cmdarg from
  verify_cmdarg to
  git_fetch_commits ("git log -z --ancestry-path --pretty=format:%H:%h:%T:%P:%B " <> from <> ".." <> to)
                    (Commits Sync Map.empty Map.empty Map.empty)

git_fetch_commits cmd commits = do
  gitDir <- askGitDir
  fd <- liftIO$ U.openFd (gitDir <> "/rehi/commits")
                 (U.WriteOnly)
                 (Just (unionFileModes ownerReadMode ownerWriteMode))
                 U.defaultFileFlags{ U.append = True }
  commits <- execStateT ((liftIO $ command_lines cmd) >>= mapM (\case
      "\n" -> pure ()
      line -> do
        git_parse_commit_line line
        liftIO $ fdWriteAll fd line
    )) commits
  liftIO $ U.closeFd fd
  pure commits

git_load_commits = do
    gitDir <- askGitDir
    execStateT (do
      mapFileLinesM git_parse_commit_line (gitDir <> "/rehi/commits") "\0"
      liftIO (fileExist (gitDir <> "/rehi/marks")) >>= \case
        True -> mapFileLinesM addMark (gitDir <> "/rehi/marks") "\n"
        False -> pure ()) commitsEmpty
  where
    addMark line = do
      case regex_match line "^([0-9a-zA-Z_\\/]+) ([0-9a-fA-F]+)$" of
        Just [_, mName, mValue] -> modify' (\c -> c{ stateMarks = Map.insert mName (Hash mValue) (stateMarks c) })
        Nothing -> fail ("Ivalid mark line: " <> show line)

git_parse_commit_line line = do
  case regex_match line "^([0-9a-f]+):([0-9a-f]+):([0-9a-f]+):([0-9a-f ]*):(.*)$" of
    Just [_, Hash -> hash, ahash, Hash -> tree, map Hash . BC.split ' ' -> parents, trim -> body] -> do
      verify_hash hash
      mapM_ verify_hash parents
      let
        (subject : _) = BC.split '\n' body
        obj = Entry ahash hash subject parents tree
      modify' (\c -> c{ stateByHash = Map.insertWith (const id) hash obj (stateByHash c)
                      , stateRefs = Map.insertWith (\hNew hOld -> if hNew == hOld then hOld else error ("Duplicated ref with different hash: " <> show ahash <> "=>" <> show hOld <> ", " <> show hNew))
                                              ahash
                                              hash
                                              (stateRefs c)})
    _ -> fail ("Could not parse line: " <> show line)

git_merge_base = undefined

verify_hash = undefined

mapFileLinesM :: MonadIO m => (ByteString -> m ()) -> ByteString -> ByteString -> m ()
mapFileLinesM func path sep = do
  fd <- liftIO $ U.openFd path U.ReadOnly Nothing U.defaultFileFlags
  mapFdLinesM func fd sep
  liftIO $ U.closeFd fd

mapFdLinesM :: MonadIO m => (ByteString -> m ()) -> Fd -> ByteString -> m ()
mapFdLinesM = undefined

commitsEmpty = Commits Sync Map.empty Map.empty Map.empty

command_lines :: ByteString -> IO [ByteString]
command_lines = undefined

verify_cmdarg = undefined

returnC x = ContT $ const x

find_sequence :: _ -> _ -> _ -> _ -> [_]
find_sequence = undefined

appendToFile = undefined

resolve_ahash = undefined

commits_get_subject = undefined

git_sequence_editor = undefined

git_no_uncommitted_changes = undefined

retry :: ExceptT EditError _m _x -> _m (Maybe _x)
retry = undefined

init_save = undefined

git_fetch_commit_list = undefined

get_env = undefined

save_todo = undefined

cleanup_save = undefined

read_file = undefined


read_todo :: _ -> _ -> ExceptT EditError _ _
read_todo = undefined

git_verify_clean = undefined

git_get_checkedout_branch = undefined

regex_match :: ByteString -> ByteString -> Maybe [ByteString]
regex_match = undefined

regex_match_all = undefined

regex_split = undefined

run_command :: ByteString -> IO ()
run_command = undefined

modifySnd f (x, y) = (x, f y)

askGitDir :: MonadReader Env m => m RawFilePath
askGitDir = ask >>= \r -> pure (envGitDir r)

fromExcept code = runExceptT code >>= \case
  Right v -> pure v
  Left e -> fail (show e)

trim = snd . (ByteString.spanEnd space) . ByteString.dropWhile space
  where
    space = (`ByteString.elem` " \t\n\r")

writeFile path content = do
  fd <- U.createFile path (unionFileModes ownerReadMode ownerWriteMode)
  fdWriteAll fd content
  U.closeFd fd

fdWriteAll fd content =
  fix (\rec content -> do
                        cnt <- fmap (fromInteger . toInteger) $ UB.fdWrite fd content
                        if cnt < ByteString.length content
                          then rec (ByteString.drop cnt content)
                          else pure ())
      content
