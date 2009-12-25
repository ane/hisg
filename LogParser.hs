module LogParser where

import Types
import Text.ParserCombinators.Parsec

-- Constants. You shouldn't touch these.
-- Parses a line into a line.
decode :: String -> Maybe Logline
decode = (either (const Nothing) Just) . (parse line "")

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

line :: CharParser st Logline
line = do
    try parseEvent
    <|> try parseUserMessage
    <|> try parseNotification
    <?> "Weird line"

parseUserMessage :: CharParser st Logline
parseUserMessage = do
    ts <- parseTimestamp
    char '<'
    status <- anyChar
    nickName <- anyChar `manyTill` (lookAhead (oneOf ">"))
    char '>'
    space
    cont <- parseContent
    return (Message ts nickName cont)

parseNotification :: CharParser st Logline
parseNotification = do
    string "--- "
    cont <- (try parseDayChange <|> parseContent)
    return (Notification cont)

parseDayChange :: CharParser st String
parseDayChange = do
    string "Day changed "
    date <- parseContent
    return ("DATE CHANGE: "++ date)

parseEvent :: CharParser st Logline
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
    return (LogEvent (Event ts evtype user host param))

parseEventType :: CharParser st EventType
parseEventType = do
    ev <- anyChar `manyTill` (lookAhead (oneOf " "))
    space
    return (case ev of
        "joined"    -> "JOIN"
        "left"        -> "PART"
        "quit"        -> "QUIT"
        _             -> "")


