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

generateHourlyActivityBarChart :: [Int] -> String
generateHourlyActivityBarChart hours = linkImg $ urlBase ++ chartData
  where
    linkImg url = "<img src=\"" ++ url ++ "\" />"
    urlBase = "http://chart.apis.google.com/chart?chbh=a,0,0&chs=140x20&cht=bhs&chco=" ++ colours ++ "&chds=0,1,0,1,0,1,0,1&chd=t:"
    chartData = intercalate "|" $ map (\x -> show (fromIntegral x / fromIntegral total)) hours
    total = sum hours
    colours = intercalate "," siteColours

--0.25,0.35|0.25,0.25|0.15,0.15|0.35,0.25
