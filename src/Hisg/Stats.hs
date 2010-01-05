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
    getDates,
    getKicks,
    calcMessageStats,
    processMessages,
    getNicks,
    ) where

import Data.List
import Data.Array (elems)
import Data.Array.ST
import Data.STRef
import Data.Maybe
import Control.Monad
import Control.Monad.ST

import Hisg.Types

instance Ord User where
  (User n1 w1 l1) <= (User n2 w2 l2) = l1 <= l2
  (User n1 w1 l1) > (User n2 w2 l2) = l1 > l2

instance Eq User where
  (User n1 w1 l1) == (User n2 w2 l2) = n1 == n2 && w1 == w2 && l1 == l2

data AnalyzerM a =  A (Log -> [(a, Log)])

runAnalyzer (A a) log = a log

-- | Tests the next event against the function f.
analyzePred :: (LogEvent -> Bool) -> AnalyzerM LogEvent
analyzePred f = A p
    where
        p [] = []
        p (x:xs) | f x = [(x, xs)]
                 | otherwise = []

event :: EventType -> AnalyzerM LogEvent
event = analyzePred . checkEvent

kick :: AnalyzerM LogEvent
kick = analyzePred isKick

message :: AnalyzerM LogEvent
message = analyzePred isMessage

checkEvent t (CustomEvent e) = eventType e == t
checkEvent _ _ = False

isKick (KickEvent _) = True
isKick _ = False

isEvent (CustomEvent _) = True
isEvent _ = False

isMessage (Message _ _ _) = True
isMessage _ = False

calcMessageStats :: Log -> [User]
calcMessageStats logf = runST $ do
    users <- newSTRef []
    forM_ (processMessages logf) $ \(n, l, w) -> modifySTRef users ((User n l w) :)
    readSTRef users

getNicks :: Log -> [String]
getNicks log = runST $ do
    users <- newSTRef []
    forM_ log $ \msg -> do
        let n = nickname msg
        l <- readSTRef users
        when (isMessage msg && n `notElem` l) $ modifySTRef users (++ [n])
    readSTRef users

processMessages :: Log -> [(String, Int, Int)
processMessages log = runST $ do
    let nicks = getNicks log
    users <- newArray (0, length nicks) ("", 0,0) ::
                ST s (STArray s Int (String, Int,Int))
    forM_ log $
        \msg ->
            when (isMessage msg) $ do
                let nick = nickname msg
                    ni = nindex nick nicks
                (n_, l_, w_) <- readArray users ni
                writeArray users ni (nick, l_ + 1, w_ + length (words (content msg)))
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
