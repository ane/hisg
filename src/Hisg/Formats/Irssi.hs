module Hisg.Formats.Irssi (
    normalMessage,
    timeStamp,
--    kickEvent,
--    topicEvent,
--    dayChangeEvent,
--    joinEvent,
--    quitEvent,
--    partEvent,
--    banEvent
    ) where

import Hisg.Types
import Hisg.Formats.Base (validNickChars)
import qualified Data.ByteString.Char8 as S


timeStamp :: String
timeStamp = "(\\d{2}:\\d{2})"

nickName :: String
nickName = "<.([" ++ validNickChars ++ "]+)>"

-- | The normalMessage represents a normal IRC channel-directed PRIVMSG, e.g.:
-- 11:00 < Kalroth> DOCTOR IS THAT MY BONE STICKING OUT AHHH
-- The capture group consists of three parts: timestamp, nickname and message.
normalMessage :: S.ByteString
normalMessage = S.intercalate (S.pack "\\s") $ map S.pack [timeStamp, nickName, "(.*)"]
