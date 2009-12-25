module User (
    countLines,
    countWords,
    countEvents,
    getEvents,
    getLines,
    getWords,
    getNicks,
    buildUsers) where

import Data.List
import Types

instance Ord User where
  (User n1 w1 l1) <= (User n2 w2 l2) = l1 <= l2
  (User n1 w1 l1) > (User n2 w2 l2) = l1 > l2

instance Eq User where
  (User n1 w1 l1) == (User n2 w2 l2) = n1 == n2 && w1 == w2 && l1 == l2

-- Counts the number of lines spoken by nick.
countLines :: String -> Logfile -> Int
countLines nick = length . getLines nick

-- Gets the number of lines spoken by nick.
getLines :: String -> Logfile -> Logfile
getLines = filter . matchNick

-- Counts the number of words spoken by nick.
countWords :: String -> Logfile -> Int
countWords nick = length . getWords nick

getWords :: String -> Logfile -> [String]
getWords nick log = foldl (++) [] (map (msgWords nick) log)

-- Gets the number of words in a line spoken by nick.
msgWords nick (Message _ nick' cont) = if nick == nick' then words cont else []
msgWords _ _ = []

-- Counts the number of events of type evt by nick.
countEvents :: String -> EventType -> Logfile -> Int
countEvents nick evt = length . getEvents nick evt

getEvents :: String -> EventType -> Logfile -> Logfile
getEvents str = filter . checkEvent str

-- Checks whether the event was correct.
checkEvent :: String -> EventType -> Logline -> Bool
-- First case: EventType is "", so we can match any event.
checkEvent nick [] (LogEvent e) = if event_user e == nick then True else False
checkEvent nick evt (LogEvent e) = if event_type e == evt && event_user e == nick then True else False
checkEvent _ _ _ = False

matchNick nick (Message ts nick' cont) = nick == nick'
matchNick _ _ = False

getNicks :: Logfile -> [String]
getNicks logfile = nub $ map (getNick) (filter isMessage logfile)
    where
        isMessage (Message _ _ _) = True
        isMessage _ = False
        getNick (Message ts nick cont) = nick
        getNick _ = ""

buildUsers :: Logfile -> [User]
buildUsers log = map buildUser (getNicks log)
    where
        buildUser nick = User nick (countWords nick log) (countLines nick log)


