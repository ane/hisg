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
import Data.Maybe
import Control.Monad
import Text.Printf
import System.IO

import Types
import Stats

type Interval = String

intervals = [("week", 7),
             ("month", 30),
             ("day", 1)]

colors :: [String]
colors = ["CCFFCC", "66CCFF", "0066FF", "0000CC"]

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
proportionPeriodHours :: [[Int]] -> (Double, Double, Double, Double)
proportionPeriodHours hours = h $ map prop hours
    where
        h [a, b, c, d] = (a, b, c, d)
        tot = sum (map length hours)
        prop [] = 0.0
        prop xs = len / fromIntegral tot
            where
                len = fromIntegral (length xs)

-- Falls back to 1 if it doesn't find a good interval.
-- Should use 7?
getInterval :: Interval -> Int
getInterval interval = fromMaybe 1 (lookup interval intervals)

analyzeHourly :: Log -> Interval -> [[Int]]
analyzeHourly log interval = calcAverages . catenateRows . chunk (getInterval interval) . toDailyHours $ getDays log
    where
        -- The moment I wrote this I knew my soul was damned forever.
        calcAverages = map (map (round . (*100) . avg))
        catenateRows = map (conv4 . unzip4)
        toDailyHours = map (proportionPeriodHours . spliceDay)

daysByWeeks xs interval = map length (map head . chunk interval $ xs)

analyzeLineTrend :: [[LogEvent]] -> Interval -> [Int]
analyzeLineTrend days interval = map propLen linesPerDay
    where
        -- Usually divided into 7 days.
        linesPerDay = daysByWeeks days (getInterval interval)
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

genLineChartUrl :: Handle -> Log -> Interval -> IO ()
genLineChartUrl out log interval = do
    let days = getDays log
        crunched = analyzeLineTrend days interval
        ylegend = intercalate "|" (map show [1 .. length crunched])
        dayLines = map length days
        biggest = maximum dayLines
        average = round $ fromIntegral (sum dayLines) / fromIntegral (length dayLines) :: Int
        prop = round (100 * (fromIntegral average / fromIntegral biggest)) :: Int

    hPutStrLn out $ "<h2>Average lines per "++interval++"</h2>" ++
                    "<img src=\"" ++
                    "http://chart.apis.google.com/chart?" ++
                    "&chm=B,66CCFF,0,1,0" ++
                    "&cht=lc&chs=500x250&chd=t:" ++
                    genDataSet crunched ++
                    "&chco=445588&chxt=y,y,x,x,r&chxl=0:|lines|1:|" ++
                    show biggest ++ "|2:|" ++ ylegend ++ "|3:|"++interval++"|4:|average " ++
                    show average ++ " lines|&chxtc=4,-500&chxp=0,100|1,100|3,50|4," ++ show prop ++
                    "&chxs=4,445588,13,-1,lt,990000\"/>"

deviations xs = map ((\x -> fromIntegral x ** 2) . flip (-) (mean xs)) xs
mean xs = round $ fromIntegral (sum xs) / fromIntegral (length xs)

genHourlyChartUrl :: Handle -> Log -> String -> IO ()
genHourlyChartUrl out log interval = do
    let weeks = analyzeHourly log interval
        cols = intercalate "," colors
        fills = map (\(idx, col) -> printf "b,%s,%d,%d,0" col (idx :: Int) (idx + 1 :: Int)) (zip [0..] colors)
        dsets = intercalate "|" . map genDataSet . reverse . conv5 . unzip5 . map (conv5' . scanl (+) 0) $ weeks
        url = printf "http://chart.apis.google.com/chart?cht=lc&chds=0,100&chs=500x250&chxt=y,x,x&chxp=0,100|2,100&chxl=0:|100%%|2:|%s|&chdl=18-24|12-18|06-12|00-06&chco=%s&chm=%s&chd=t:%s" interval cols (intercalate "|" fills) dsets
    hPutStrLn out "<h2>Monthly activity distribution by hours</h2>"
    hPutStrLn out $ "<img src=\"" ++ url ++ "\"/>"
