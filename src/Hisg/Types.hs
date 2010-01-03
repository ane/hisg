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

module Hisg.Types where

import Data.List
import Data.Char
import Text.Printf

-- Basic types
data Timestamp = Timestamp { ts_hour :: Int, ts_minute :: Int }
data Date = Date { day :: Int, month :: Int, year :: Int }
data User = User { userNick :: String, userWords :: Int, userLines :: Int }
data Event = Event { eventTimestamp :: Timestamp, eventType :: EventType, eventUser :: String, eventHost :: String, eventParam :: String }

data Mode = Mode { modeTS :: Timestamp, modeChars :: String, authorHost :: String, targets :: String }
data Topic = Topic { topicTs :: Timestamp, topicAuthor :: String, topicContent :: String }
data Kick = Kick { kickTs :: Timestamp, kickAuthor :: String, kickTarget :: String, kickReason :: String }

data LogEvent =
    Message { timestamp :: Timestamp, nickname    :: String, content :: String }
    | Notification String
    | DateChange Date
    | CustomEvent Event
    | ModeChange Mode
    | TopicChange Topic
    | KickEvent Kick
    | Simple String

-- Type aliases

type Log = [LogEvent]
data EventType = Join | Part | Quit | Nick | Unknown deriving (Eq, Show)

months :: [String]
months = ["Jan",
          "Feb",
          "Mar",
          "Apr",
          "May",
          "Jun",
          "Jul",
          "Aug",
          "Sep",
          "Oct",
          "Nov",
          "Dec"]

instance Show Timestamp where
    show (Timestamp h m) = printf "%02d:%02d" h m

instance Show LogEvent where
    show (Message ts nick content) = show ts ++ " " ++ "<" ++ nick ++ ">" ++ " " ++ content
    show (Notification cont) = cont
    show (DateChange (Date d m y)) = intercalate " " (map show [d, m, y])
    show (CustomEvent ev) = show ev
    show (Simple str) = str
    show (KickEvent (Kick ts author target reason)) = show ts ++ " " ++ author ++ " kicked " ++ target ++ ", reason: " ++ reason

instance Show User where
    show (User nick words lines) = nick ++ " :: " ++ show words ++ " words, " ++ show lines ++ " lines"

instance Show Event where
    show (Event ts evtype evuser host param) = map (toUpper) (show ts) ++ " " ++ show evtype ++ ": " ++ evuser ++ " (" ++ host ++ ")" ++ " -> " ++ param
