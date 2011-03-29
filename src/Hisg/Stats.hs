-- hisg - IRC stats generator.
--
-- Copyright (c) 2009, 2010 Antoine Kalmbach <antoine dot kalmbach at jyu dot fi>
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--   * Redistributions of source code must retain the above copyright
--   notice, this list of conditions and the following disclaimer.
--   * Redistributions in binary form must reproduce the above copyright
--   notice, this list of conditions and the following disclaimer in the
--   documentation and/or other materials provided with the distribution.
--   * Neither the name of the author nor the
--   names of its contributors may be used to endorse or promote products
--   derived from this software without specific prior written permission.
--
-- For further details, see LICENSE.

module Hisg.Stats where

import Data.List
import Control.Parallel.Strategies
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Text.Regex.PCRE.Light (compile, match)

import Hisg.Types
import Hisg.MapReduce
import Hisg.Formats.Irssi

--instance Ord User where
--  (User n1 w1 l1) <= (User n2 w2 l2) = l1 <= l2
--  (User n1 w1 l1) > (User n2 w2 l2) = l1 > l2
--
--instance Eq User where
--(User n1 w1 l1) == (User n2 w2 l2) = n1 == n2 && w1 == w2 && l1 == l2

type UserStats = (Int, Int, Int)
type StatsMap = M.Map S.ByteString UserStats


-- | Calculates statistics for an user, currently lines, words and kicks given.
-- Will eventually calculate everything you can use integers for, i.e.
-- joins, quits, parts etc.
calcUserStats :: [L.ByteString] -> StatsMap
calcUserStats = mapReduce rseq (foldl' matchAll M.empty . L.lines)
                   rseq (M.unionsWith sumTriples)

matchAll :: StatsMap                       -- our line
         -> L.ByteString  -- our map
         -> StatsMap  -- our modified map if the line matches
matchAll m l = matchKick l . matchMessage l $ m

matchMessage :: L.ByteString                      -- our line
             -> StatsMap  -- our map
             -> StatsMap  -- our modified map if the line matches
matchMessage line statsMap = case match (compile normalMessageRegex []) (conv line) [] of
  Just (_:_:nick:contents)
    -> M.insertWith' sumTriples nick (1, length . S.words . S.concat $ contents, 0) statsMap
  _ -> statsMap

matchMessage :: L.ByteString                      -- our line
             -> StatsMap  -- our map
             -> StatsMap  -- our modified map if the line matches
matchKick line map = case match (compile kickMessageRegex []) (conv line) [] of
  Just (_:_:_:_:nick:_)
    -> M.insertWith' sumTriples nick (0, 0, 1) map
  _ -> map

conv = S.concat . L.toChunks

sumTriples (a,b,c) (d,e,f) = (a+d, b+e, c+f)

