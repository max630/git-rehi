{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Rehi.Regex where

import Control.Monad.Fix(fix)
import Data.ByteString(ByteString)
import Data.String(IsString,fromString)
import System.IO.Unsafe (unsafePerformIO)
import Text.Regex.PCRE.ByteString (compile, regexec, compBlank, execBlank)

import qualified Text.Regex.PCRE as P

newtype Regex = Regex ByteString deriving (Show,Eq,Monoid)

instance IsString Regex where
  fromString s = Regex (fromString s)

regex_match_ex :: P.CompOption -> ByteString -> Regex -> Maybe [ByteString]
regex_match_ex op str (Regex pattern) = unsafePerformIO match
  where
    match = do
      re <- compile1 op pattern
      re str >>= (pure . fmap (\(_, self, _, groups) -> self : groups))

regex_match = regex_match_ex compBlank

regex_match_with_newlines = regex_match_ex P.compDotAll

compile1 op pat = do
  compile op execBlank pat >>= \case
    Left (_, msg) -> error ("regex compile: " ++ msg ++ ", pat=" ++ show pat)
    Right re -> pure $ \str -> regexec re str >>= \case
      Left (_, msg) -> error ("regex run: " ++ msg)
      Right result -> pure result

regex_match_all :: ByteString -> Regex -> [ByteString]
regex_match_all str (Regex pat) = unsafePerformIO match
  where
    match = do
      re <- compile1 compBlank pat
      fix (\ret rest parsed ->
            if rest == ""
              then pure parsed
              else re rest >>= \case
                Just ("", _, rest, [next]) -> ret rest (parsed ++ [next])
                _ -> error "regex_match_all: chunk not matched"
          ) str []

regex_split :: ByteString -> ByteString -> [ByteString]
regex_split content pat = unsafePerformIO match
  where
    match = do
      re <- compile1 compBlank pat
      fix (\next content result ->
              if content == ""
                then pure result
                else re content >>= \case
                  Just (chunk, _, rest, _) -> next rest (result ++ [chunk])
                  Nothing -> pure (result ++ [content]))
          content []
