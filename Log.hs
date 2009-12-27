-- Hessu - IRC stats generator.
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

module Log where

import Types
import Text.ParserCombinators.Parsec
import Data.List.Split (splitOn)
import Data.List

-- Constants. You shouldn't touch these.
-- Parses a line into a line.
decode :: String -> Maybe LogEvent
decode = either (const Nothing) Just . parse line ""

-- What defines our line. Usually it's "timestamp event data", the timestamp is ubiquituous.

parseTimestamp :: CharParser st Timestamp
parseTimestamp = do
    h <- many1 digit
    char ':'
    m <- many1 digit
    space
    return (Timestamp (read h) (read m))

parseContent :: CharParser st String
parseContent = anyChar `manyTill` (lookAhead (oneOf "\n"))

line :: CharParser st LogEvent
line = do
    try parseEvent
    <|> try parseKick
    <|> try parseDayChange
    <|> try parseUserMessage
    <|> try parseNotification
    <?> "Weird line"

parseUserMessage :: CharParser st LogEvent
parseUserMessage = do
    ts <- parseTimestamp
    char '<'
    status <- anyChar
    nickName <- anyChar `manyTill` lookAhead (oneOf ">")
    char '>'
    space
    cont <- parseContent
    return (Message ts nickName cont)

parseNotification :: CharParser st LogEvent
parseNotification = do
    string "--- "
    cont <- parseContent
    return (Notification cont)

parseDayChange :: CharParser st LogEvent
parseDayChange = do
    string "--- Day changed "
    date <- parseContent
    let items = words date
        [m', d', y'] = drop 1 items
        m = case elemIndex m' months of
                Just x -> x
                Nothing -> 0
        d = read d'::Int
        y = read y'::Int
    return (DateChange (Date d m y))

parseKick :: CharParser st LogEvent
parseKick = do
    ts <- parseTimestamp
    string "-!-"
    space
    target <- many1 letter
    space
    string "was kicked from "
    char '#'
    anyChar `manyTill` lookAhead (oneOf " ")
    string " by "
    author <- many1 letter
    space
    char '['
    reason <- anyChar `manyTill` lookAhead (oneOf "]")
    char ']'
    return (KickEvent (Kick ts author target reason))

parseEvent :: CharParser st LogEvent
parseEvent = do
    ts <- parseTimestamp
    string "-!-"
    space
    user <- many1 letter
    space
    char '['
    host <- anyChar `manyTill` (lookAhead (oneOf "]"))
    char ']'
    string " has "
    evtype <- parseEventType
    param <- parseContent
    return (CustomEvent (Event ts evtype user host param))

parseEventType :: CharParser st EventType
parseEventType = do
    ev <- anyChar `manyTill` (lookAhead (oneOf " "))
    space
    return (case ev of
        "joined"    -> "JOIN"
        "left"        -> "PART"
        "quit"        -> "QUIT"
        "changed"     -> "NICK"
        _             -> "")


