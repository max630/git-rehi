{-# LANGUAGE OverloadedStrings #-}
module TryO where

import Control.Monad.Trans.Writer (runWriterT)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.Except (runExceptT)

import Options.Applicative
import Options.Applicative.Types

import Rehi
import Rehi.ParserT

o = flag' Abort (long "abort")
    <|> flag' Continue (long "continue")
    <|> flag' Skip (long "skip")
    <|> flag' Current (long "current")
    <|> Run "a" (Just "b") ["c"] (Just "d") <$> argument auto (metavar "DEST") <*> pure False

try1 = execParserPure (prefs mempty) (info (helper <*> o) fullDesc)

-- try2 :: Monad m => [String] -> ParserT m CliMode
try2 args = runReaderT (runWriterT $ runExceptT $ (runParserFully AllowOpts o args :: Monad m => ParserT m CliMode)) (prefs mempty)
