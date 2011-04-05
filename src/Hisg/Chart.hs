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

module Hisg.Chart where

import Data.List
import Data.List.Split
import Data.Maybe
import Control.Monad
import Text.Printf
import System.IO

import Hisg.Misc

siteColours :: [String]
siteColours = ["330570", "7C1F7C", "996AD6", "8243D6"]

generateCharsToLinesRatio :: [(String, Int, Int)] -> String
generateCharsToLinesRatio wl =
    linkImg $ urlBase ++ chartData ++ nameMarkers wL

  where

    urlBase = "http://chart.apis.google.com/chart?chxl=0:|More characters|1:|More lines&chxp=0,100|1,100&chxs=0,313186,11.5,0,l,4F4FD9|1,313186,11.5,0,lt,4F4FD9&chxt=x,y&chs=640x320&cht=s&chco=330570|313184&chds="++show (negate (fromIntegral (mostChars wL) * 0.25))++","++show (fromIntegral (mostChars wL) * 1.25)++","++show (negate (fromIntegral (mostLines wL) * (0.25)))++","++show (fromIntegral (mostLines wL) * 1.25)++",0,100&chf=bg,s,F9FAF4&chg=10,10&chtt=&chts=6C006C,17.5&chd=t:"

    chartData = wordData ++ "|" ++ lineData
    wL = unzip3 wl
    lines_ (_, l, _) = l
    chars (_, _, c) = c
    mostLines (_, l, _) = maximum (lines_ wL)
    leastLines (_, l, _) = minimum (lines_ wL)
    mostChars (_, _, c) = maximum (chars wL)
    leastChars (_, _, c) = minimum (chars wL)

    nameMarkers :: ([String], [Int], [Int]) -> String
    nameMarkers (nicks, _, _) = "&chm=d,6C006C,0,4,5|" ++ intercalate "|" (map makeMarker (zip [0..] nicks))
    makeMarker :: (Int, String) -> String
    makeMarker (idx, nick) = printf "t%s,000000,0,%d,10,,h:0:4" nick idx

    lineData = intercalate "," (map show (lines_ wL))
    wordData = intercalate "," (map show (chars wL))

generateChannelHourlyActivityBarChart :: [Int] -> String
generateChannelHourlyActivityBarChart [] = ""
generateChannelHourlyActivityBarChart hours = linkImg $ urlBase ++ chartData
  where
    urlBase = "http://chart.apis.google.com/chart?chxr=0,0,"++show (maximum hours)++"&chxs=0,49188F,11.5,0,l,444444|1,49188F,11.5,0,lt,676767&chxt=x&chxr=0,0,23&chbh=a,4,0&chs=640x320&cht=bvs&chco="++colours++"&chds=0,"++show ((mostActiveHour + mostActiveHour*0.1))++"&chm=N*p*,003045,0,0:23,9,,::4&chg=0,15&chtt=&chf=bg,s,F9FAF4&chts=6C006C,18.5&chd=t:"
    chartData = intercalate "," (map (\h -> printf "%f" (h :: Float)) percentages)
    totalLines = sum hours
    mostActiveHour = maximum percentages
    percentages = map (\x -> (fromIntegral x / fromIntegral totalLines)) hours
    colours = intercalate "|" (concat (crevf [0..23] siteColours))

generateUserHourlyActivityBarChart :: [Int] -> String
generateUserHourlyActivityBarChart hours = linkImg $ urlBase ++ chartData
  where
    urlBase = "http://chart.apis.google.com/chart?chbh=a,0,0&chs=140x20&cht=bhs&chco=" ++ colours ++ "&chds=0,1,0,1,0,1,0,1&chf=bg,s,F9FAF4&chd=t:"
    chartData = intercalate "|" $ map (\x -> show (fromIntegral x / fromIntegral total)) hours
    total = sum hours
    colours = intercalate "," siteColours

linkImg url = "<img src=\"" ++ url ++ "\" />"
