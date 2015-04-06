-- hisg - Haskell IRC stats generator.
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

import           Control.Monad.State.Lazy
import           Data.Maybe
import           Data.Time.Clock
import           Hisg.Formatter
import           Hisg.IRCLog

-- | The Hisg state monad that implements the link between user input and program execution.
type HisgM = StateT Hisg IO

data Hisg = Hisg { files :: [IRCLog] }

data Command = Parse | Format

data HisgChange = ParseFile IRCLog
                | FormatFile IRCLog

-- | Adds a log file into the parsing bucket.
addFile :: IRCLog -> HisgM ()
addFile ircLog = modify (Hisg . (:) ircLog . files)

-- | Gets a file from the parsing bucket. Names are unique.
getFile :: String -> HisgM (Maybe IRCLog)
getFile n = do
    hst <- get
    return $ listToMaybe $ filter ((==) n . filename) (files hst)

-- | Loads a file and queues it for parsing.
loadFile :: String -> HisgM ()
loadFile inp = do
    logf <- liftIO $ do
      putStrLn $ "Processing " ++ inp ++ "..."
      loadLog inp
    addFile logf

-- | Formats and writes each analyzed file.
processFiles :: UTCTime -> (String -> IRCLog -> NominalDiffTime -> FormatterM String) -> HisgM ()
processFiles startTime formatter = do
    fileBucket <- files `fmap` get
    mapM_ (liftIO . churn) fileBucket
    where
      churn f = processLog f startTime formatter


-- | Formats and writes the output to a file using the formatter.
processLog :: IRCLog -> UTCTime -> (String -> IRCLog -> NominalDiffTime -> FormatterM String) -> IO ()
processLog ircLog operationStartTime formatterFunc = do
    let fn = takeWhile ('.' /=) $ filename ircLog
        out = fn ++ ".html"
    putStr $ "Formatting " ++ filename ircLog ++ "..."
    endTime <- getCurrentTime
    derp <- let elapsed = diffUTCTime endTime operationStartTime
            in evalStateT (formatterFunc fn ircLog elapsed) (Formatter "" . userScores $ ircLog)
    putStrLn " done."
    putStr $ "Writing " ++ out ++ "..."
    writeFile out derp
    putStrLn " done."


