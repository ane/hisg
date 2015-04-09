{-# LANGUAGE OverloadedStrings #-}
module Match
       ( match )
       where

import           Hisg.Analysis.Event

match :: String -> Maybe Event
match str =
  case str of
    "hello" -> Just $ Message "ane" str
    "barp"  -> Just $ Kick "ane" "ena" "una"

