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

import Text.Printf

import Hisg.Stats
import Hisg.Misc
import Hisg.Chart
import Hisg.User

import qualified Data.Map as M
import qualified Data.ByteString.Char8 as S

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
    addOutput $ "</div><div id=\"footer\"><div id=\"footercontent\"><p>Generated with <a href=\"http://ane.github.com/hisg\">hisg</a> v" ++ ver ++ "</p></div></div></body></html>"

insertScoreboard :: [(S.ByteString, UserStats)] -> FormatterM ()
insertScoreboard users = do
    addOutput "<h2>Top 15 users</h2>"
    addOutput $ "<table>\n<tr><th></th><th>Nickname</th><th>Lines</th><th>Words</th><th>Ratio</th><th>Activity by hour</th></tr>"
        ++ concatMap (\(rank, (nick, stats)) ->
        let lineC = head (fst stats)
            wordC = (fst stats !! 1)
            ratio = (fromIntegral wordC / fromIntegral lineC) in
           "<tr><td><b>"
        ++ show rank ++ ".</b></td><td> "
        ++ S.unpack nick ++ "</td><td>" ++ show lineC
        ++ "<td>" ++ show (fst stats !! 1) ++ "</td>"
        ++ "<td>" ++ printf "%.02f" (ratio :: Float) ++ "</td>"
        ++ "<td>"
        ++ generateHourlyActivityBarChart (hourlyActivityToList (snd stats))
        ++ "</td>"
        ++ "</tr>") (zip [1..] users) ++ "</table>"

hourlyActivityToList :: HourStats -> [Int]
hourlyActivityToList m = sums (M.toList m)
  where
    sums m_ = take 4 (map (sum . snd . unzip) (chunk 6 m_))

openPanel :: FormatterM ()
openPanel = addOutput "<div class=\"panel\">"

closePanel :: FormatterM ()
closePanel = addOutput "</div>"

insertWordsToLinesRatio :: [(Int, Int)] -> FormatterM ()
insertWordsToLinesRatio wl = addOutput $ generateWordsToLinesRatio wl

insertKickScoreboard :: [(S.ByteString, UserStats)] -> FormatterM ()
insertKickScoreboard [] = do openPanel; addOutput "Nobody kicked anyone in the channel."; closePanel
insertKickScoreboard ((fist, ([_, _, kicks], _)):_) = do
  openPanel
  addOutput ("<b>" ++ S.unpack fist ++ "</b>")
  addOutput " ruled with an iron fist. He kicked <b>"
  addOutput (show kicks)
  addOutput "</b> people out of the channel!"
  closePanel



