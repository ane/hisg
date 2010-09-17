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

import qualified Data.ByteString.Lazy.Char8 as S

-- | The FormatterM monad provides a data abstraction layer between the formatted content
--  and user input. @addOutput@ and @getOutput@ are the methods used to add and fetch data,
--  respectively.
--
--  The intent is that one can just pipe output in a chain and keep the output pure, but liftable
--  into IO. Maybe this whole design is completely redundant, I don't know.
--
-- TODO: Turn this to use WriterT. Using StateT for appending is slow.
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
             ++ "</style>\n<body><div id=\"head\">\n"
             ++ "<h1>Statistics for #" ++ takeWhile (/= '.') chan ++ "</h1></div><div id=\"main\">"

-- | Adds a small HTML footer.
insertFooter :: String -> FormatterM ()
insertFooter ver = do
    addOutput $ "</div><div id=\"footer\"><p>Generated with <a href=\"http://ane.github.com/hisg\">hisg</a> v" ++ ver ++ "</p></div></body></html>"

insertScoreboard :: [User] -> FormatterM ()
insertScoreboard users = do
    addOutput "<h2>Top 15 users</h2>"
    addOutput $ "<table>\n<tr><th>Nickname</th><th>Lines</th><th>Words</th></tr>" ++ concatMap (\(rank, u) -> "<tr><td><b>" ++ show rank ++ ".</b> " ++ S.unpack (userNick u) ++ "</td><td>" ++ show (userLines u) ++ "</td><td>" ++ show (userWords u) ++ "</td></tr>") (zip [1..] users) ++ "</table>"
