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

{-# OPTIONS_GHC -cpp #-}
{-# LANGUAGE CPP #-}

module Main where

import Data.List
import Data.Maybe
import System
import System.Console.GetOpt
import System.IO

import Data.List.Split (splitWhen)

import Hisg.Types
import Hisg.Log
import Hisg.Stats
import Hisg.Misc
import Hisg.Formatter
import Hisg.Chart

version = "0.1.0"

showVersion = do putStrLn $ "hisg v" ++ version
exactVersion = do putStrLn $ "compiled on " ++ __DATE__ ++ " at " ++ __TIME__

header = "Usage: hisg INPUT OUTPUT"

parseInput :: String -> Log
parseInput inp = map (fromMaybe (Simple "") . (decode . (++ "\n"))) (lines inp)

--parseInput :: String -> Log
--parseInput inp = map (decode . (++ "\n")) (lines inp)

splitOnDates :: Log -> [[LogEvent]]
splitOnDates = splitWhen (isDate)
    where
        isDate (DateChange _) = True
        isDate _ = False

buildOutput input output = do
    let interval = "month"
    showVersion
    putStr $ "Opening file " ++ input ++ "... "
    infile <- openFile input ReadMode
    inp <- hGetContents infile
    putStrLn "success. Analyzing (this might take a while)."
    putStr "Compiling stats... "
    let decoded = parseInput inp
    putStrLn "done."
    out <- openFile output WriteMode
    writeHeaders out input "style.css" decoded
    putStr "Writing pertinent graphs and tables... "
    genLineChartUrl out decoded interval
    writeUsersTable out (take 25 (reverse . sort $ getUserStats decoded))
    putStrLn "done."
    putStr "Writing detailed graphs... "
    genHourlyChartUrl out decoded interval
    putStrLn "done."
    putStr "Writing miscellaneous stats... "
    writeMiscStats out decoded
    putStrLn "done."
    hPutStrLn out (footer version)
    putStrLn "Completed."
    hClose out


main = do
    argv <- getArgs
    case argv of
        []              -> buildOutput "sekopaat.log" "seko.html" --showVersion >> putStrLn header
        ["-v"]          -> showVersion >> exactVersion
        ["--version"]   -> showVersion >> exactVersion
        _               -> buildOutput (head argv) (last argv)

    --file <- readFile "suomipelit.log"
    --let decoded = map (fromMaybe (Simple "") . (decode . (++ "\n"))) (lines file)
    --putStrLn (intercalate "\n" (map (show) (reverse $ qsort (getUserStats decoded))))
    --headers "suomipelit.com"
    --    --putStrLn $ "Generated by " ++ version
    --footer
    --
