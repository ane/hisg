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

module Hisg.Formatter where

import System.IO
import Data.List
import Control.Monad.State.Lazy
import Data.Maybe
import Data.List
import System.IO

import Hisg.Types
import Hisg.Stats
import Hisg.Misc


-- | The FormatterM monad provides a data abstraction layer between the formatted content
-- | and user input. @addOutput@ and @getOutput@ are the methods used to add and fetch data,
-- | respectively.
type FormatterM = StateT Formatter IO

data Formatter = Formatter { output :: String }

-- | Adds output to the content.
addOutput :: String -> FormatterM()
addOutput str = do
    fmst <- get
    put $ Formatter $ (output fmst) ++ str

-- | Gets the final output.
getFinalOutput :: FormatterM String
getFinalOutput = do
    fmst <- get
    return (output fmst)

-- | Adds HTML headers to the output.
insertHeaders :: String -> FormatterM ()
insertHeaders chan = do
    addOutput str
    where
        str =   "<html>\n<head><title>Statistics for #" ++ chan ++ "</title>"
             ++ "<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" />"
             ++ "</style>\n<body>\n"
             ++ "<h1>Statistics for #" ++ takeWhile (/= '.') chan ++ "</h1>"

-- | Adds a small HTML footer.
insertFooter :: String -> FormatterM ()
insertFooter ver = do
    addOutput $ "<p>Generated with <a href=\"http://anhekalm.github.com/hisg\">hisg</a> v" ++ ver ++ "</p></body></html>"

insertScoreboard :: [User] -> FormatterM ()
insertScoreboard users = do
    addOutput "<h2>Top 25 users</h2>"
    addOutput $ "<table>\n<tr><th>Nickname</th><th>Number of lines</th><th>Number of words</th></tr>" ++ concatMap (\(rank, u) -> "<tr><td><b>" ++ show rank ++ ".</b> " ++ userNick u ++ "</td><td>" ++ show (userLines u) ++ "</td><td>" ++ show (userWords u) ++ "</td></tr>") (zip [1..] users) ++ "</table>"

-- FOR THE LOVE OF GOD, MAKE THIS CODE BETTER.
writeMiscStats :: Handle -> Log -> IO ()
writeMiscStats out logf = do
    let kicks = getKicks logf
        kickers = common (map getKicker kicks)
        kickeds = common (map getKicked kicks)

    hPutStrLn out "<h2>Miscellaneous stats</h2>\n<table>"
    if length kickers > 0
        then do
            let topkicker = head kickers
                topkicked = head kickeds
            hPutStrLn out $ "<tr><td><b>" ++ head topkicker ++ "</b> acted as the channel judge. He kicked a total of <b>" ++ show (length topkicker) ++ "</b> people!<br/>"
            if length kickers > 1
                then do
                    let sndkicker = kickers !! 1
                    hPutStrLn out $ "His lieutenant, <b>" ++ head sndkicker ++ "</b>, assisted with <b>" ++ show (length sndkicker) ++ "</b> kicks!</td></tr>"
                else do
                    hPutStrLn out "No one else kicked people in the channel."
            hPutStrLn out $ "<tr><td>Nobody liked <b>" ++ head topkicked ++ "</b>. He got kicked " ++ show (length topkicked) ++ "</b> times!<br/>"
            if length kickeds > 1
                then do
                    let sndkicked = kickeds !! 1
                    hPutStrLn out $ "<b>" ++ head sndkicked ++ "</b> came second on the loser chart, getting kicked " ++ show (length sndkicked) ++ "</b> times!<br/></td></tr>"
                else do hPutStrLn out "He was the only one to get kicked!"
        else do hPutStrLn out "<tr><td>Nobody kicked anyone during this period.</td></tr>"

    hPutStrLn out "</table>"

getKicked (KickEvent k) = kickTarget k
getKicker (KickEvent k) = kickAuthor k

