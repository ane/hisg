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
module Hisg.Core where

import Control.Concurrent.STM
import Control.Monad.State.Lazy
import Data.Maybe
import Data.List
import System.IO

import Hisg.IRCLog
import Hisg.Formatter
import Hisg.Stats

-- | The Hisg state monad that implements the link between user input and program execution.
type HisgM = StateT Hisg IO

data Hisg = Hisg { files :: [IRCLog] }

data HisgChange = ParseFile IRCLog
                | FormatFile IRCLog

-- | Adds a log file into the parsing bucket.
addFile :: IRCLog -> HisgM ()
addFile l = do
    hst <- get
    put $ Hisg $ l : (files hst)

-- | Gets a file from the parsing bucket. Names are unique.
getFile :: String -> HisgM (Maybe IRCLog)
getFile n = do
    hst <- get
    return $ listToMaybe $ filter (\logf -> filename logf == n) (files hst)

-- | Parses a log file according to the user commands.
parseLog :: String -> HisgM (Maybe HisgChange)
parseLog fn = do
    logf <- getFile fn
    return $ do
        l <- logf
        Just $ ParseFile l

-- | Formats a log into a HTML file.
formatLog :: String -> String -> HisgM (Maybe HisgChange)
formatLog fn outf = do
    logf <- getFile fn
    return $ do
        l <- logf
        Just $ FormatFile l

-- | Loads a file and queues it for parsing.
loadFile :: String -> HisgM ()
loadFile inp = do
    logf <- liftIO $ do
        putStrLn $ "Load: " ++ inp
        loadLog inp
    addFile logf

processFiles :: HisgM ()
processFiles = do
    hst <- get
    forM_ (files hst) $ \logf -> liftIO $ writeLog logf

writeLog :: IRCLog -> IO ()
writeLog logf = do
    let fn = takeWhile ('.' /=) $ filename logf
        out = fn ++ ".html"
    putStr $ "Writing " ++ out ++ "..."
    outf <- openFile out WriteMode
    writeUsersTable outf (take 25 (reverse . sort $ calcMessageStats (contents logf)))
    hClose outf
    putStrLn " done."
