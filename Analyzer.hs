module Analyzer (countLines, countWords, countEvents, getEvents, getLines, getWords, runAnalyzer) where

import Data.List
import Types

data Analyzer t = A (Logfile -> t)

runAnalyzer (A a) file = a file

-- Counts the number of lines spoken by nick.
countLines :: String -> Analyzer Int
countLines nick = A fun
  where
    fun []  = 0
    fun log = length (getLines nick log)

-- Gets the number of lines spoken by nick.
getLines :: String -> Logfile -> Logfile
getLines nick log = filter (matchNick nick) log

-- Counts the number of words spoken by nick.
countWords :: String -> Analyzer Int
countWords nick = A fun
  where
    fun []  = 0
    fun log = length (getWords nick log)

getWords :: String -> Logfile -> [String]
getWords nick log = foldl (++) [] (map (msgWords nick) log)

-- Gets the number of words in a line spoken by nick.
msgWords nick (Message _ nick' cont) = if nick == nick' then words cont else []
msgWords _ _ = []

-- Counts the number of events of type evt by nick.
countEvents :: String -> EventType -> Analyzer Int
countEvents nick evt = A fun
  where
    fun [] = 0
    fun log = length (getEvents nick evt log)

getEvents :: String -> EventType -> Logfile -> Logfile
getEvents str evt log = filter (checkEvent str evt) log

-- Checks whether the event was correct.
checkEvent :: String -> EventType -> Logline -> Bool
-- First case: EventType is "", so we can match any event.
checkEvent nick [] (LogEvent e) = if event_user e == nick then True else False
checkEvent nick evt (LogEvent e) = if event_type e == evt && event_user e == nick then True else False
checkEvent _ _ _ = False

matchNick nick (Message ts nick' cont) = nick == nick'
matchNick _ _ = False
