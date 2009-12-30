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
    matchNick,
    getNicks,
    isMessage,
    getUserLines,
    getUserWords,
    getUserStats,
    getDates,
    getKicks
    ) where

import Data.List

import Hisg.Types

instance Ord User where
  (User n1 w1 l1) <= (User n2 w2 l2) = l1 <= l2
  (User n1 w1 l1) > (User n2 w2 l2) = l1 > l2

instance Eq User where
  (User n1 w1 l1) == (User n2 w2 l2) = n1 == n2 && w1 == w2 && l1 == l2

matchNick nick (Message _ nick' _) = nick == nick'
matchNick _ _ = False

getNicks :: Log -> [String]
getNicks logfile = nub [ nick | Message _ nick _ <- filter isMessage logfile]

isMessage (Message _ _ _) = True
isMessage _ = False

getUserLines :: String -> Log -> [String]
getUserLines nick logf = map content (filter match logf)
    where
        match line = isMessage line && matchNick nick line

getUserWords :: [String] -> [String]
getUserWords = concatMap words

getUserStats :: Log -> [User]
getUserStats logf = buildU `fmap` getNicks logf
    where
        buildU nick = let ls = getUserLines nick logf in User nick (length (getUserWords ls)) (length ls)

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
