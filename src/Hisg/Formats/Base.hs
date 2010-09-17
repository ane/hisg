module Hisg.Formats.Base (
    validNickChars
    ) where

import qualified Data.ByteString.Char8 as S

-- | Valid nickname characters according to IRC RFC 2812 are A-Z, a-z, {}[]`^\|.
validNickChars :: String
validNickChars = "[A-Za-z\\[\\]\\\\`_\\^\\{\\|\\}"
