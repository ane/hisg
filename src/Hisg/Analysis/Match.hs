module Match
       ( match )
       where

import Hisg.Analysis.Event

match :: String -> Event Message
match "hello" = Msg $ Message "ding" "dong"
