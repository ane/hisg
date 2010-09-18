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
import Hisg.Formats.Base
import qualified Data.ByteString.Char8 as S


-- | The timestamp is the time at which any event occurred.
timeStamp :: String
timeStamp = "(\\d{2}:\\d{2})"

nickName :: String
nickName = "<.([" ++ validNickChars ++ "]+)>"

channelName :: String
channelName = "[#&!+]([" ++ validChanChars ++ "]+)"

-- | The normalMessage represents a normal IRC channel-directed PRIVMSG, e.g.:
-- 11:00 < Kalroth> DOCTOR IS THAT MY BONE STICKING OUT AHHH
-- The capture group consists of three parts: timestamp, nickname and message.
normalMessage :: S.ByteString
normalMessage = S.intercalate (S.pack "\\s") $ map S.pack [timeStamp, nickName, "(.*)"]

-- | The kick is a kick, containing four elements: kick target, kicker, channel and the reason, e.g.:
-- 07:00 -!- rzz_ was kicked from #elitistjerks by CrazyDazed [Rule 1: Don't be stupid]
kick :: S.ByteString
kick = S.intercalate (S.pack "\\s") $ map S.pack [timeStamp, "-!-", nickName, "was kicked from", channelName, "\\[(.*)\\]"]
