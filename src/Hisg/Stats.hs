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
--   getNicks,
    ) where

import Data.List
import Data.Array (elems)
import Data.Array.ST
import Data.STRef
import Data.Maybe
import Control.Monad
import Control.Monad.ST
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as S

import Hisg.Types
import Hisg.MapReduce

instance Ord User where
  (User n1 w1 l1) <= (User n2 w2 l2) = l1 <= l2
  (User n1 w1 l1) > (User n2 w2 l2) = l1 > l2

instance Eq User where
  (User n1 w1 l1) == (User n2 w2 l2) = n1 == n2 && w1 == w2 && l1 == l2

isMessage (Message _ _ _) = True
isMessage _ = False

calcMessageStats :: Log -> [User]
calcMessageStats logf = runST $ do
    users <- newSTRef []
    forM_ (processMessages logf) $ \(n, l, w) -> modifySTRef users ((User n l w) :)
    readSTRef users

-- | Gets the unique nicknames of all log messages in the log
{-
getNicks' :: Log -> [S.ByteString]
getNicks' log = nub users
    where
        users = catMaybes $ map nick log
        nick (Message _ n _) = Just n
        nick _ = Nothing

getNicks'' :: Log -> [S.ByteString]
getNicks'' log = Set.toList nicks
    where
        nicks = Set.fromList $ catMaybes $ map nick log
        nick (Message _ n _) = Just n
        nick _ = Nothing

getNicks :: Log -> [S.ByteString]
getNicks log = runST $ do
    users <- newSTRef []
    forM_ log $ \msg -> do
        let n = nickname msg
        l <- readSTRef users
        when (isMessage msg && n `notElem` l) $ modifySTRef users (++ [n])
    readSTRef users
-}
processMessages :: Log -> [(S.ByteString, Int, Int)]
processMessages log = map fmt $ M.toList $ mapReduce rwhnf (foldl' update M.empty)
                                                     rwhnf (M.unionsWith (sumTuples)) (map msgs log)
    where
        -- Increments the user word count by wc and line count by 1.
        update map (Message _ nick line) = M.insertWith (sumTuples) nick (1, S.count ' ' line) map
        msgs = filter isMessage
        sumTuples (a,b) (c,d) = (a+c,b+d)
        fmt (a, (b,c)) = (a, c, b)
{-
-- Old STArray implementation
processMessages' :: Log -> [(S.ByteString, Int, Int)]
processMessages' log = runST $ do
    let nicks = getNicks log
    users <- newArray (0, length nicks) (S.pack "", 0,0) ::
                ST s (STArray s Int (S.ByteString, Int,Int))
    forM_ log $
        \msg ->
            when (isMessage msg) $ do
                let nick = nickname msg
                    ni = nindex nick nicks
                (n_, l_, w_) <- readArray users ni
                writeArray users ni (nick, l_ + 1, w_ + length (S.words (content msg)))
    getElems users

    where
        nindex i = fromMaybe 0 . (i `elemIndex` )

getDates :: Log -> [LogEvent]
getDates = filter isDate
    where
        isDate (DateChange _) = True
        isDate _ = False

getKicks :: Log -> [LogEvent]
getKicks = filter isKick
    where
        isKick (KickEvent _) = True
        isKick _ = False
-}
