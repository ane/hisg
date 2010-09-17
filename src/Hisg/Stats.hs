-- hisg - IRC stats generator.
--
-- Copyright (c) 2009, 2010 Antoine Kalmbach <antoine dot kalmbach at jyu dot fi>
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--     * Neither the name of the author nor the
--       names of its contributors may be used to endorse or promote products
--       derived from this software without specific prior written permission.
--
-- For further details, see LICENSE.

module Hisg.Stats (
    isMessage,
--    getDates,
--    getKicks,
    calcMessageStats,
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
import qualified Data.ByteString.Lazy.Char8 as S

import Hisg.Types
import Hisg.MapReduce

instance Ord User where
  (User n1 w1 l1) <= (User n2 w2 l2) = l1 <= l2
  (User n1 w1 l1) > (User n2 w2 l2) = l1 > l2

instance Eq User where
  (User n1 w1 l1) == (User n2 w2 l2) = n1 == n2 && w1 == w2 && l1 == l2

-- | The StatsM monad allows us to work on a changing set of log events.
-- As a result, we can prune what we want from the log while calculating stats,
-- thus reducing the size of the log file by not having to travel through
-- every bit of (un-lazy) data.
type StatsM = State Log

-- | Gets the messages from the current log.
getMessages :: StatsM Log
getMessages = do
    evts <- get
    put $ map (filter (not . isMessage)) evts
    return $ map (filter isMessage) evts

isMessage (Message _ _ _) = True
isMessage _ = False

workMessageStats :: StatsM [User]
workMessageStats = do
    msgs <- getMessages
    return $ map toUser (processMessages msgs)

calcMessageStats :: Log -> [User]
calcMessageStats log = evalState (workMessageStats) log

toUser (n, l, w) = User n l w

processMessages :: Log -> [(S.ByteString, Int, Int)]
processMessages log = map fmt $ M.toList $ mapReduce rwhnf (foldl' updateWLC M.empty)
                                                     rwhnf (M.unionsWith (sumTuples)) log
    where
        fmt (a, (b, c)) = (a, c, b)

-- | Alias for insertWith (it's shorter!)
updateMap :: (Ord k) => (a -> a -> a) -> k -> a -> M.Map k a -> M.Map k a
updateMap fn key value = M.insertWith fn key value

updateWLC :: M.Map S.ByteString (Int, Int) -> LogEvent -> M.Map S.ByteString (Int, Int)
updateWLC map (Message ts nick line) = updateMap sumTuples nick (1, length $ S.words line) map
updateWLC map _ = map

sumTuples (a,b) (c,d) = (a+c, b+d)

