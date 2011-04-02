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
addFile l = do
    hst <- get
    put $ Hisg $ l : files hst

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
processFiles :: HisgM ()
processFiles = do
    hst <- get
    mapM_ (liftIO . processLog) (files hst)

compareIthJth :: (Ord k) => Int -> Int -> [k] -> [k] -> Ordering
compareIthJth i j xs ys = compare jth ith
  where
    ith = xs !! i
    jth = ys !! j

wordsLines :: [(S.ByteString, UserStats)] -> [(Int, Int)]
wordsLines wl = let getWLtuple (_, ([l, w, _], _)) = (l, w) in map getWLtuple wl

-- | Formats a log file, producing HTML ouput.
formatLog :: String -> IRCLog -> FormatterM String
formatLog chan logf = do
    let messagePopular (_, (aList, _)) (_, (bList, _)) = compareIthJth 0 0 aList bList
        kickPopular (_, (aList, _)) (_, (bList, _)) = compareIthJth 2 2 aList bList
        scoreList = M.toList . userScores $ logf
        topMessages = sortBy messagePopular scoreList

    insertHeaders chan

    insertScoreboard (take 15 topMessages)

    insertKickScoreboard (sortBy kickPopular (filter (\(_, ([_, _, k], _)) -> k > 0) scoreList))
    insertWordsToLinesRatio (wordsLines . take 15 $ topMessages)
    insertHourlyActivity scoreList
    insertFooter "0.1.0"
    getFinalOutput

-- | Formats and writes the output to a file.
processLog :: IRCLog -> IO ()
processLog logf = do
    let fn = takeWhile ('.' /=) $ filename logf
        out = fn ++ ".html"
    putStr $ "Formatting " ++ filename logf ++ "..."
    output <- evalStateT (formatLog fn logf) (Formatter "")
    putStrLn " done."
    putStr $ "Writing " ++ out ++ "..."
    outf <- openFile out WriteMode
    hPutStrLn outf output
    hClose outf
    putStrLn " done."
