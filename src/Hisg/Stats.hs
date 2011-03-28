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

module Hisg.Stats (
  isMessage,
--  getDates,
--  getKicks,
  calcMessageStats,
  calcMessageStats'',
  calcKickStats,
  processMessages,
  updateMap
--   getNicks,
  ) where

import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.State
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

-- | The StatsM monad allows us to work on a changing set of log events.
-- As a result, we can prune what we want from the log while calculating stats,
-- thus reducing the size of the log file by not having to travel through
-- every bit of (un-lazy) data.
type StatsM = State Log

-- | Gets the messages from the current log and updates the state with the remaining non-messages.
takeMessages :: StatsM Log
takeMessages = do
  evts <- get
  put $ map snd $ (map (partition isMessage) evts)
  return $ map fst $ (map (partition isMessage) evts)

isMessage (Message _ _ _) = True
isMessage _ = False

workMessageStats :: StatsM [(S.ByteString, (Int, Int))]
workMessageStats = do
  msgs <- takeMessages
  return $ processMessages msgs

--workKickStats :: StatsM [(S.ByteString, Int)]
--workKickStats = diio
--  msgs <- takeKicks
--
calcMessageStats :: Log -> [(S.ByteString, (Int, Int))]
calcMessageStats log = processMessages log

-- | Calculates statistics for normal messages, returning a map
-- with stats for each nick.
calcMessageStats'' :: [L.ByteString] -> M.Map S.ByteString (Int, Int)
calcMessageStats'' = mapReduce rwhnf (foldl' updateWLC M.empty . L.lines)
                   rwhnf (M.unionsWith (sumTuples))
  where
    updateWLC map line =
      case match (compile normalMessage []) (conv line) [] of
        Just (_:ts:nick:contents)
              -> M.insertWith' (sumTuples) nick (1, length . S.words . S.concat $ contents) map
        _ -> map

conv = S.concat . L.toChunks

-- | The tuple records kicks and kicks received
calcKickStats :: [L.ByteString] -> M.Map S.ByteString Int
calcKickStats = mapReduce rwhnf (foldl' updateKC M.empty . L.lines)
                          rwhnf (M.unionsWith (+))
  where
    updateKC map line =
        case match (compile kick []) (conv line) [] of
            Just (_:ts:target:chan:author:reason)
                -> M.insertWith' (+) author 1 map
            _ -> map

--calcKickStats :: Log -> [(S.ByteString, Int)]
--calcKickStats = evalState (workKickStats)

--toUser (n, l, w) = User n l w
--toUser' (n, l) = User n l 0

processMessages :: Log -> [(S.ByteString, (Int, Int))]
processMessages log = M.toList $ mapReduce rwhnf (foldl' updateWLC M.empty)
                                             rwhnf (M.unionsWith (sumTuples)) log
processMessages' :: Log -> [(S.ByteString, Int)]
processMessages' log = M.toList $ mapReduce rwhnf (foldl' updateWLC' M.empty)
                                             rwhnf (M.unionsWith (+)) log

-- | Alias for insertWith (it's shorter!)
updateMap :: (Ord k) => (a -> a -> a) -> k -> a -> M.Map k a -> M.Map k a
updateMap fn key value = M.insertWith fn key value

updateWLC :: M.Map S.ByteString (Int, Int) -> LogEvent -> M.Map S.ByteString (Int, Int)
updateWLC map (Message ts nick line) = updateMap sumTuples nick (1, length $ S.words line) map
updateWLC map _ = map

updateWLC' :: M.Map S.ByteString Int -> LogEvent -> M.Map S.ByteString Int
updateWLC' map (Message ts nick line) = updateMap (+) nick 1 map
updateWLC' map _ = map

sumTuples (a,b) (c,d) = (a+c, b+d)

