module Event where

-- Counts the number of events of type evt by nick.
countEvents :: String -> EventType -> Logfile -> Int
countEvents nick evt = length . getEvents nick evt

getEvents :: String -> EventType -> Logfile -> Logfile
getEvents str = filter . checkEvent str

-- Checks whether the event was correct.
checkEvent :: String -> EventType -> Logline -> Bool
-- First case: EventType is "", so we can match any event.
checkEvent nick [] (LogEvent e) = event_user e == nick
checkEvent nick evt (LogEvent e) = event_type e == evt && event_user e == nick
checkEvent _ _ _ = False



