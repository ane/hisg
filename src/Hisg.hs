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

{-# OPTIONS_GHC -cpp -fglasgow-exts #-}
{-# LANGUAGE CPP #-}

module Main where

import Data.Maybe
import System.Console.GetOpt
import System.Environment (getArgs)

import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Lazy

import Hisg.Core

version_ = "0.1.0"

version = "hisg v" ++ version_
exactVersion = version ++ " compiled on " ++ __DATE__ ++ " at " ++ __TIME__

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

-- | Parses the command line options and then processes files (if any).
runHisg :: [String] -> HisgM ()
runHisg [] = loadFile "sekopaat.log" >> processFiles -- TEST LOG, REPLACE WITH USAGE
runHisg args = do
    runArgs args
    processFiles

runArgs :: [String] -> HisgM ()
runArgs args =
    case (getOpt (ReturnInOrder File) options args) of
        (o, [], []) -> mapM_ decide o
        (_, _, errs) -> fail (concat errs)

decide :: Opts -> HisgM ()
decide opt =
    case opt of
        Help -> liftIO $ putStrLn usage
        Version -> liftIO $ putStrLn exactVersion
        File fn -> loadFile fn

main = do
    args <- getArgs
    runStateT (runHisg args) (Hisg [])

