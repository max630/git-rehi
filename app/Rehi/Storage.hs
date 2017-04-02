{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
module Rehi.Storage where

import Rehi.Types(Step(..),EditError(..),Commits(..))

import Control.Monad.Catch(MonadMask,MonadThrow,SomeException(SomeException),Exception,Handler(Handler),throwM)
import Control.Monad.Fix(fix)
import Control.Monad.IO.Class(liftIO,MonadIO)
import Control.Monad.RWS(execRWST, RWST, runRWST)
import Control.Monad.State(put,get,modify',MonadState)
import Control.Monad.Writer(tell)
import Data.ByteString(ByteString)
import Data.Monoid((<>))

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as BC

import Rehi.Utils (mapFileLinesM, ifM)
import Rehi.Utils.IO (removeFile, doesFileExist)
import Rehi.Utils.Regex (regex_match, regex_match_with_newlines, regex_match_all, regex_split)

dropCurrent :: (MonadIO m) => ByteString -> m ()
dropCurrent dir = liftIO (removeFile (dir `mappend` "/rehi/current"))

readCurrent :: (MonadIO m, MonadMask m) => ByteString -> Commits -> m (Maybe Step)
readCurrent gitDir commits = ifM (liftIO (doesFileExist (gitDir <> "/rehi/current")))
                (do
                  [step] <- read_todo (gitDir <> "/rehi/current") commits
                  pure (Just step))
                (pure Nothing)

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
