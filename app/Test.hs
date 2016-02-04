module Test where

import Rehi

import Data.ByteString.Builder (toLazyByteString, word64HexFixed, string7)
import Data.ByteString.Lazy (toStrict)
import Data.Monoid ((<>))

import qualified Data.Map as M

main = undefined

hashes = map (toStrict . toLazyByteString . (string7 (replicate 24 '0') <>) . word64HexFixed) [0 ..]

i2c (n,ps) = (Hash h, Entry h (Hash h) h phs (Hash h) h)
  where
    h = hashes !! n
    phs = map (Hash . (hashes !!)) ps

test_brs commits from to throughs =
  build_rebase_sequence
    (Commits Sync M.empty M.empty (M.fromList $ map i2c commits))
    (Hash (hashes !! from))
    (Hash (hashes !! to))
    (map (Hash . (hashes !!)) throughs)

-- pick 0
brs1 = test_brs [(0, [1])] 1 0 []

-- empty
brs2 = test_brs [(1, [0]),(2, [1])] 2 2 []

-- p 1, mark, p 2, reset, p 3, merge
brs3 = test_brs [(1, [0]),(2, [1]),(3,[1]),(4,[2,3])] 0 4 []
