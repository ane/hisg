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
--

-- | Implements the Hisg state monad, used to contain log files to be parsed
-- | and the actions to do with them.
--

module Hisg.Core where

import Control.Monad.State.Lazy
import Data.Maybe
import Data.List
import System.IO
import qualified Data.Map as M

import qualified Data.ByteString.Char8 as S
import Hisg.IRCLog
import Hisg.Formatter
import Hisg.Stats

-- | The Hisg state monad that implements the link between user input and program execution.
type HisgM = StateT Hisg IO

data Hisg = Hisg { files :: [IRCLog] }

data Command = Parse | Format

data HisgChange = ParseFile IRCLog
                | FormatFile IRCLog

-- | Adds a log file into the parsing bucket.
addFile :: IRCLog -> HisgM ()
addFile ircLog = do
    oldFiles <- files `liftM` get
    put $ Hisg $ ircLog : oldFiles

-- | Gets a file from the parsing bucket. Names are unique.
getFile :: String -> HisgM (Maybe IRCLog)
getFile n = do
    hst <- get
    return $ listToMaybe $ filter (\logf -> filename logf == n) (files hst)

-- | Loads a file and queues it for parsing.
loadFile :: String -> HisgM ()
loadFile inp = do
    logf <- liftIO $ do
              putStrLn $ "Processing " ++ inp ++ "..."
              loadLog inp
    addFile logf

-- | Formats and writes each analyzed file.
processFiles :: (String -> IRCLog -> FormatterM String) -> HisgM ()
processFiles formatter = do
    fileBucket <- files `fmap` get
    mapM_ (\x -> liftIO (processLog x formatter)) fileBucket


-- | Formats and writes the output to a file using the formatter.
processLog :: IRCLog -> (String -> IRCLog -> FormatterM String) -> IO ()
processLog ircLog formatterFunc = do
    let fn = takeWhile ('.' /=) $ filename ircLog
        out = fn ++ ".html"
    putStr $ "Formatting " ++ filename ircLog ++ "..."
    output <- let stats = userScores ircLog in
              evalStateT (formatterFunc fn ircLog) (Formatter "" stats)
    putStrLn " done."
    putStr $ "Writing " ++ out ++ "..."
    outf <- openFile out WriteMode
    hPutStrLn outf output
    hClose outf
    putStrLn " done."
