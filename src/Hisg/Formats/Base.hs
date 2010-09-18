module Hisg.Formats.Base (
    validNickChars,
    validChanChars
    ) where

import qualified Data.ByteString.Char8 as S

-- | Valid nickname characters according to IRC RFC 2812 are A-Z, a-z, {}[]`^\|.
validNickChars :: String
validNickChars = "A-Za-z\\[\\]\\\\`_\\^\\{\\|\\}"

-- | Valid channel characters are "any octet except NUL, BELL, CR, LF, " ", "," and ":""
validChanChars :: String
validChanChars = "^\\r\\n\\0\\s,:"
