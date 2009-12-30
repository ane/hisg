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

module Event (
        countEvents,
        getEvents,
        checkEvent
       ) where

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



