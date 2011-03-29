module Hisg.Formats.Irssi (
    normalMessageRegex,
    timeStampRegex,
    kickMessageRegex,
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
timeStampRegex :: String
timeStampRegex = "(\\d{2}:\\d{2})"

nickNameRegex :: String
nickNameRegex = "([" ++ validNickChars ++ "]+)"

messageNickNameRegex = "<." ++ nickNameRegex ++ ">"

channelNameRegex :: String
channelNameRegex = "[#&!\\+]([" ++ validChanChars ++ "]+)"

-- | The normalMessage represents a normal IRC channel-directed PRIVMSG, e.g.:
-- 11:00 < Kalroth> DOCTOR IS THAT MY BONE STICKING OUT AHHH
-- The capture group consists of three parts: timestamp, nickname and message.
normalMessageRegex :: S.ByteString
normalMessageRegex = S.intercalate (S.pack "\\s") $ map S.pack [timeStampRegex, messageNickNameRegex, "(.*)"]

-- | The kick is a kick, containing four elements: kick target, kicker, channel and the reason, e.g.:
-- 07:00 -!- rzz_ was kicked from #elitistjerks by CrazyDazed [Rule 1: Don't be stupid]
kickMessageRegex :: S.ByteString
kickMessageRegex = S.intercalate (S.pack "\\s") $ map S.pack [timeStampRegex, "-!-", nickNameRegex, "was kicked from", channelNameRegex, "by", nickNameRegex, "\\[(.*)\\]"]
