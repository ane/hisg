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

import Hisg.MapReduce
import Hisg.Formats.Irssi

-- | lines, words, kicks, night, morning, afternoon, evening
type UserStats = [Int]
type StatsMap = M.Map S.ByteString UserStats


-- | Calculates statistics for an user, currently lines, words and kicks given.
-- Will eventually calculate everything you can use integers for, i.e.
-- joins, quits, parts etc.
calcUserStats :: [L.ByteString] -> StatsMap
calcUserStats = mapReduce rseq (foldl' matchAll M.empty . L.lines)
                   rseq (M.unionsWith sumList)

matchAll :: StatsMap                       -- our line
         -> L.ByteString  -- our map
         -> StatsMap  -- our modified map if the line matches
matchAll m l = matchKick l . matchMessage l $ m

matchMessage :: L.ByteString -> StatsMap -> StatsMap  -- our modified map if the line matches
matchMessage line statsMap = case match (compile normalMessageRegex []) (conv line) [] of
  Just (_:ts:nick:contents)
    -> let hours = getHours ts in M.insertWith' sumList nick [1, length . S.words . S.concat $ contents, 0, night hours, morning hours, afternoon hours, evening hours] statsMap
  _ -> statsMap
  where
    getHours ts = case (S.readInt ts) of
                    Just (x, _) -> x
                    _ -> 0
    night h | 0 <= h && h < 6 = 1
            | otherwise = 0
    morning h | 6 <= h && h < 12 = 1
              | otherwise = 0
    afternoon h | 12 <= h && h < 18 = 1
                | otherwise = 0
    evening h | 18 <= h && h < 24 = 1
              | otherwise = 0

matchKick :: L.ByteString                      -- our line
             -> StatsMap  -- our map
             -> StatsMap  -- our modified map if the line matches
matchKick line map = case match (compile kickMessageRegex []) (conv line) [] of
  Just (_:_:_:_:nick:_)
    -> M.insertWith' sumList nick [0, 0, 1, 0, 0, 0, 0] map
  _ -> map

conv = S.concat . L.toChunks

sumList = zipWith (+)

-- I should REALLY REALLY (YES) implement this stuff as a list. but oh well.
sumN (a, b, c, d, e, f, g) (a', b', c', d', e', f', g') = (a+a', b+b', c+c', d+d', e+e', f+f', g+g')
sumTriples (a,b,c) (d,e,f) = (a+d, b+e, c+f)

