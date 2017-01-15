{-# LANGUAGE FlexibleInstances #-}
module Rehi.ParserT where

import Control.Monad.Trans.Writer (WriterT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Except (ExceptT, throwE, catchE)
import Control.Monad.Writer.Class (tell)
import Control.Monad.Reader.Class (ask)
import Options.Applicative.Internal (MonadP, setContext, getPrefs, missingArgP, tryP, errorP, exitP, Context(Context))
import Options.Applicative.Types (ParserPrefs, ParseError(ShowHelpText))

type ParserT m = ExceptT ParseError (WriterT Context (ReaderT ParserPrefs m))

instance Monad m => MonadP (ParserT m) where
  setContext s i = tell (Context (maybe [] (:[]) s) i)
  getPrefs = ask
  missingArgP e _ = throwE e
  tryP p = catchE (Right <$> p) (pure . Left)
  errorP = throwE
  exitP p r = maybe (errorP $ ShowHelpText) pure r

