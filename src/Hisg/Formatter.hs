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

module Hisg.Formatter where

import System.IO
import Data.List
import Control.Monad.State.Lazy
import Data.Maybe
import Data.List
import System.IO

import Text.Printf

import Hisg.Stats
import Hisg.Misc
import Hisg.Chart
import Hisg.User

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as S

-- | The FormatterM monad provides a data abstraction layer between the formatted content
--  and user input. @addOutput@ and @getOutput@ are the methods used to add and fetch data,
--  respectively.
--
--  The intent is that one can just pipe output in a chain and keep the output pure, but liftable
--  into IO. Maybe this whole design is completely redundant, I don't know.
--
-- TODO: Turn this to use WriterT. Using StateT for appending is slow.
type FormatterM = StateT Formatter IO

data Formatter = Formatter { output :: String, stats :: StatsMap }

-- | Adds output to the content.
addOutput :: String -> FormatterM()
addOutput str = do
    fmst <- get
    put $ Formatter ((output fmst) ++ str) (stats fmst)

-- | Gets the final output.
getFinalOutput :: FormatterM String
getFinalOutput = output `fmap` get

-- | Adds HTML headers to the output.
headers :: String -> FormatterM ()
headers chan =
    addOutput $ "<html>\n<head><title>Statistics for #" ++ chan ++ "</title>"
             ++ "<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" />"
             ++ "</style>\n<body><div id=\"head\">\n"
             ++ "<h1>Statistics for #" ++ takeWhile (/= '.') chan ++ "</h1></div><div id=\"main\">"

-- | Adds a small HTML footer using @ver@ as the version number.
footer :: String -> FormatterM ()
footer ver =
    addOutput $ "</div><div id=\"footer\"><div id=\"footercontent\"><p>Generated with <a href=\"http://ane.github.com/hisg\">hisg</a> v" ++ ver ++ "</p></div></div></body></html>"

-- | Inserts a scoreboard where @n@ is the number of users shown. If n is zero
--   it defaults to 15.
scoreboard :: Int -> FormatterM ()
scoreboard n | n == 0 = scoreboard 15
             | otherwise = do
    userStats <- (take n . topMessages . M.toList . stats) `fmap` get
    addOutput $
        "<h2>Top 15 users</h2>" ++
        "<table>\n<tr><th></th><th>Nickname</th><th>Lines</th><th>Characters per line</th><th>Activity by hour</th></tr>"
        ++
        concatMap (\(rank, (nick, ([lineC, wordC, _], hours))) ->
        let ratio = (fromIntegral wordC / fromIntegral lineC) in
            "<tr><td><b>"
            ++ show rank ++ ".</b></td><td> "
            ++ S.unpack nick ++ "</td><td>" ++ show lineC
            ++ "<td>" ++ printf "%.02f" (ratio :: Float) ++ "</td>"
            ++ "<td>"
            ++ generateUserHourlyActivityBarChart (hourlyActivityToList hours)
            ++ "</td>"
            ++ "</tr>") (zip [1..] userStats) ++ "</table><p>" ++ "</p>"

hourlyActivityToList :: HourStats -> [Int]
hourlyActivityToList m = sums (M.toList m)
  where
    sums m_ = take 4 (map (sum . snd . unzip) (chunk 6 m_))

-- | Unions the hourly data of all users, producing a map where the key is
--   an hour and the value is the number of lines spoken during that hour,
--   allowing us to generate a chart of hourly activity.
sumHours :: [(S.ByteString, UserStats)] -> HourStats
sumHours userStats = M.unionsWith (+) (snd (unzip (getUserHourData userStats)))

-- | Extracts user hour stats from an user tuple.
getUserHourData :: [(S.ByteString, UserStats)] -> [(S.ByteString, HourStats)]
getUserHourData = map getHourStats
  where
    getHourStats (n, (_, hs)) = (n, hs)

openPanel :: FormatterM ()
openPanel = addOutput "<div class=\"panel\">"

closePanel :: FormatterM ()
closePanel = addOutput "</div>"

messagePopular (_, (aList, _)) (_, (bList, _)) = compareIthJth 0 0 aList bList
kickPopular (_, (aList, _)) (_, (bList, _)) = compareIthJth 2 2 aList bList
topMessages = sortBy messagePopular

charsToLinesRatio :: FormatterM ()
charsToLinesRatio = do
    ratios <- (charsLines . take 15 . topMessages . M.toList . stats) `fmap` get
    addOutput $ generateCharsToLinesRatio ratios

hourlyActivity :: FormatterM ()
hourlyActivity = do
    let getHourValues = snd . unzip . M.toList . sumHours . M.toList
    hourValues <- (getHourValues . stats) `fmap` get
    addOutput $ generateChannelHourlyActivityBarChart hourValues

kickScoreboard :: [(S.ByteString, UserStats)] -> FormatterM ()
kickScoreboard [] = do openPanel; addOutput "Nobody kicked anyone in the channel."; closePanel
kickScoreboard ((fist, ([_, _, kicks], _)):_) = do
    openPanel
    addOutput ("<b>" ++ S.unpack fist ++ "</b>")
    addOutput " ruled with an iron fist. He kicked <b>"
    addOutput (show kicks)
    addOutput "</b> people out of the channel!"
    closePanel

-- | Compares the @j@th and @i@th indices of an int array. TODO: why do I [have to] do this.
--   I am not good at compu^Whaskell.
compareIthJth :: (Ord k) => Int -> Int -> [k] -> [k] -> Ordering
compareIthJth i j xs ys = compare jth ith
  where
    ith = xs !! i
    jth = ys !! j

-- | Um... giggle.
charsLines :: [(S.ByteString, UserStats)] -> [(String, Int, Int)]
charsLines wl = let getCLtriple (nick, ([l, c, _], _)) = (S.unpack nick, l, c) in map getCLtriple wl
