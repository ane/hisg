{-# LANGUAGE OverloadedStrings #-}
module Main where
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

import           Data.Time.Clock
import           System.Console.GetOpt
import           System.Environment       (getArgs)

import           Control.Monad.State.Lazy

import           Hisg.Core
import           Hisg.Formatter
import           Hisg.IRCLog


version_ :: String
version_ = "hisg v0.1.0"

data Opts = Help
          | Version
          | File String

options :: [OptDescr Opts]
options =
    [ Option "v"     ["version"]      (NoArg Version)     "Show version information",
      Option "h"     ["help"]         (NoArg Help)        "Show this help" ]

-- | Usage string.
usage :: String
usage = usageInfo "Usage: hisg [option...] INPUTFILES" options

-- | Formats a log file, producing HTML ouput.
layout :: String -> IRCLog -> NominalDiffTime -> FormatterM String
layout chan logfile elapsedTime = do
    headers chan
    scoreboard 15
    hourlyActivity
    charsToLinesRatio
    footer $ "0.1.0 in " ++ show elapsedTime
    getFinalOutput

-- | Parses the command line options and then processes files (if any).
runHisg :: [String] -> HisgM ()
runHisg [] = do
    currentTime <- liftIO getCurrentTime
    loadFile "sekopaat.log"
    processFiles currentTime layout -- TEST LOG, REPLACE WITH USAGE
runHisg args = do
    currentTime <- liftIO getCurrentTime
    runArgs args
    processFiles currentTime layout

runArgs :: [String] -> HisgM ()
runArgs args =
    case getOpt (ReturnInOrder File) options args of
        (o, [], []) -> mapM_ decide o
        (_, _, errs) -> fail (concat errs)

decide :: Opts -> HisgM ()
decide opt =
    case opt of
        Help -> liftIO $ putStrLn usage
        Version -> liftIO $ putStrLn version_
        File fn -> loadFile fn

main :: IO ()
main = do
  args <- getArgs
  _ <- runStateT (runHisg args) (Hisg [])
  return ()

bar :: IO ()
bar = putStrLn "foo"
