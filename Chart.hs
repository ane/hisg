-- Hessu - IRC stats generator.
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

module Chart where

import Data.List
import Data.List.Split
import Control.Arrow
import Text.Printf
import System.IO

import Types
import Stats

colors :: [String]
colors = ["CC9900", "445588", "990000", "336600"]

getDays :: Log -> [[LogEvent]]
getDays = splitWhen isDate
    where
        isDate (DateChange _) = True
        isDate _ = False

getMonths :: Log -> [[[LogEvent]]]
getMonths = chunk 30 . getDays


getTimestamps :: [LogEvent] -> [Timestamp]
getTimestamps events = map getTs (filter isMessage events)
    where
        getTs (Message ts _ _) = ts

getHours = map ts_hour

avg [] = 0
avg xs = sum xs / genericLength xs

-- Splices a day into four segments by hours (0-6, 6-12, 12-18, 18-24)
spliceDay :: [LogEvent] -> [[Int]]
spliceDay log = [a, b, c, d]
    where
        a = filter (<6) hours
        b = filter (\h -> h >= 6 && h < 12) hours
        c = filter (\h -> h >= 12 && h < 18) hours
        d = filter (>= 18) hours
        hours = getHours . getTimestamps $ log

-- Proportions the number of lines spoken in a segment to the total number of lines spoken that day.
proportionHours :: [[Int]] -> (Int, (Double, Double, Double, Double))
proportionHours hours = (tot, h $ map prop hours)
    where
        h [a, b, c, d] = (a, b, c, d)
        tot = sum (map length hours)
        prop [] = 0.0
        prop xs = len / fromIntegral tot
            where
                len = fromIntegral (length xs)

analyzeHourly :: Int -> Log -> [[Int]]
analyzeHourly interval log = calcAverages . catenateRows . chunk interval . toDailyHours $ (getDays log)
    where
        -- The moment I wrote this I knew my soul was damned forever.
        calcAverages = map (map (round . (*100) . avg))
        catenateRows = map (conv4 . unzip4 . snd . unzip)
        toDailyHours = map (proportionHours . spliceDay)

daysByWeeks xs interval = map length (map head . chunk interval $ xs)

analyzeLineTrend :: [[LogEvent]] -> Int -> [Int]
analyzeLineTrend days interval = map propLen linesPerDay
    where
        -- Usually divided into 7 days.
        linesPerDay = daysByWeeks days interval
        biggest = maximum linesPerDay
        propLen x = round (100 * (fromIntegral x / fromIntegral biggest))

-- Why do I have to write something like this?
conv5 (a, b, c, d, e) = [a, b, c, d, e]
conv4 (a, b, c, d) = [a, b, c, d]
conv5' [a, b, c, d, e] = (a, b, c, d, e)
conv4' [a, b, c, d] = (a, b, c, d)

genDataSet :: [Int] -> String
genDataSet [] = ""
genDataSet xs = intercalate "," (map show xs)

genLineChartUrl out log = do
    let days = getDays log
        crunched = analyzeLineTrend days 30
        ylegend = intercalate "|" (map show [1 .. length crunched])
        dayLines = map length days
        biggest = maximum dayLines
        average = (round $ fromIntegral (sum dayLines) / fromIntegral (length dayLines)) :: Int
        url = printf "http://chart.apis.google.com/chart?cht=lc&chs=500x250&chm=B,008B8B,0,0,0&chd=t:%s&chco=445588&chxt=y,y,x,x,r&chxl=0:|lines|1:|%d|2:|%s|3:|Month|4:|average %d lines|&chxtc=4,-500&chxp=0,100|1,100|3,50|4,%d&chxs=4,445588,13,-1,lt,990000" (genDataSet crunched) biggest ylegend average ((round (100 * (fromIntegral average / fromIntegral biggest))) :: Int)
    hPutStrLn out "<h2>Average lines per month</h2>"
    hPutStrLn out $ "<img src=\"" ++ url ++ "\"/>"

genHourlyChartUrl out log = do
    let weeks = analyzeHourly 30 log
        cols = intercalate "," colors
        fills = map (\(idx, col) -> printf "b,%s,%d,%d,0" col (idx::Int) ((idx+1)::Int)) (zip [0..] colors)
        dsets = intercalate "|" . map genDataSet . reverse . conv5 . unzip5 . map conv5' . map (scanl (+) 0) $ weeks
        url = printf "http://chart.apis.google.com/chart?cht=lc&chds=0,100&chs=500x250&chxt=y,x,x&chxp=0,100|2,100&chxl=0:|100%%|2:|Month|&chdl=18-24|12-18|06-12|00-06&chco=%s&chm=%s&chd=t:%s" cols (intercalate "|" fills) dsets
    hPutStrLn out "<h2>Monthly activity distribution, by hours</h2>"
    hPutStrLn out $ "<img src=\"" ++ url ++ "\"/>"
