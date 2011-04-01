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

siteColours :: [String]
siteColours = ["330570", "7C1F7C", "996AD6", "8243D6"]


generateWordsToLinesRatio :: [(Int, Int)] -> String
generateWordsToLinesRatio wl = linkImg $ urlBase ++ chartData
  where
    urlBase = "http://chart.apis.google.com/chart?chxl=0:|Lines|1:|Words&chxp=0,100|1,100&chxs=0,313186,11.5,0,l,4F4FD9|1,313186,11.5,0,lt,4F4FD9&chxt=x,y&chs=300x225&cht=s&chco=330570|313184&chds=0,"++show mostLines++",0,"++show mostWords++",0,100&chg=10,10&chtt=Words+per+line+distribution&chts=6C006C,17.5&chm=d,6C006C,0,4,5&chd=t:"
    chartData = wordData ++ "|" ++ lineData
    wL = unzip wl
    mostWords = maximum (fst wL)
    mostLines = maximum (snd wL)
    lineData = intercalate "," (map show (fst wL))
    wordData = intercalate "," (map show (snd wL))

generateHourlyActivityBarChart :: [Int] -> String
generateHourlyActivityBarChart hours = linkImg $ urlBase ++ chartData
  where
    urlBase = "http://chart.apis.google.com/chart?chbh=a,0,0&chs=140x20&cht=bhs&chco=" ++ colours ++ "&chds=0,1,0,1,0,1,0,1&chd=t:"
    chartData = intercalate "|" $ map (\x -> show (fromIntegral x / fromIntegral total)) hours
    total = sum hours
    colours = intercalate "," siteColours

linkImg url = "<img src=\"" ++ url ++ "\" />"
--0.25,0.35|0.25,0.25|0.15,0.15|0.35,0.25
