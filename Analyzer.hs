module Analyzer where

import Types

countLines :: String -> [Logline] -> Int
countLines nick log = length matched
    where
        matched = filter (matchNick nick) log

matchNick nick (Message ts nick' cont) = nick == nick'
matchNick _ _ = False
