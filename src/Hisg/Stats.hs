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
import Data.Maybe
import Control.Parallel.Strategies
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Text.Regex.PCRE.Light (compile, match)

import Text.Printf

import Hisg.MapReduce
import Hisg.Formats.Irssi

import Control.Parallel (pseq)
import Control.DeepSeq

instance NFData S.ByteString where
    rnf _ = ()    -- not built into Control.Parallel.Strategies

-- | An empty hourly distribution map.
emptyHourStats :: M.Map S.ByteString Int
emptyHourStats = M.fromList (zip (map (\x -> S.pack (printf "%02d" (x::Int))) [0 .. 23]) (repeat 0))

-- | lines, words, kicks, night, morning, afternoon, evening
type UserStats = ([Int], HourStats)
type HourStats = M.Map S.ByteString Int
type StatsMap = M.Map S.ByteString UserStats

-- | Calculates statistics for an user, currently lines, words and kicks given.
--   I am not sure whether I'd want to calculate anything more than daily and
--   monthly trends (besides user word count and word to line ration etc.), as
--   that is not really pertinent. Activity analysis is where it's at, hell yes.
calcUserStats :: [L.ByteString] -> StatsMap
calcUserStats = mapReduce rseq (foldl' matchAll M.empty . L.lines)
                   rseq (M.unionsWith sumUser)

-- | Chains all matches together. TODO: implement this in a non-stupid way.
matchAll :: StatsMap -> L.ByteString -> StatsMap
matchAll m line = let converted = conv line in
                  fromMaybe (fromMaybe m (matchKick converted m)) (matchMessage converted m)

-- | Increases the message line count and word count and modifies an users's hour distribution
--   should the regexp match.
matchMessage :: S.ByteString -> StatsMap -> Maybe StatsMap  -- our modified map if the line matches
matchMessage line statsMap = case match (compile normalMessageRegex []) line [] of
  Just (_:hour:nick:contents:_)
    -> Just $ M.insertWith' (incMessage hour) nick newValue statsMap
      where
        newValue = ([1, contents `pseq` S.length contents, 0], M.adjust succ hour emptyHourStats)
  _ -> Nothing

-- | Increases the kick count of a user if the regex matches.
matchKick :: S.ByteString -> StatsMap -> Maybe StatsMap
matchKick line map = case match (compile kickMessageRegex []) line [] of
  Just (_:_:_:_:nick:_)
    -> Just $ map `deepseq` M.insertWith' incKick nick ([0, 0, 1], M.empty) map
  _ -> Nothing

conv = S.concat . L.toChunks

incKick :: UserStats -> UserStats -> UserStats
incKick _ ([l, w, k], ts) = ([l, w, succ k], ts)

incMessage :: S.ByteString -> UserStats -> UserStats -> UserStats
incMessage ts ([_, wc', _], _) ([lc, wc, kc], hs) = wc `pseq` wc' `pseq` lc `pseq` ([succ lc, wc+wc', kc], incHour hs)
  where
    incHour = ts `pseq` M.adjust succ ts

-- | Joins two sets of user data into one.
sumUser :: UserStats -> UserStats -> UserStats
sumUser (xs@[l, w, k], hs) (xs'@[l', w', k'], hs') = (zipWith (+) xs xs', M.unionWith (+) hs hs')

