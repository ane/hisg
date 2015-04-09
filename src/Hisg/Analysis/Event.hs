{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Hisg.Analysis.Event
       ( Event(..)
       ) where

data Event = Message { author   :: String
                     , contents :: String }
             | Kick { target :: String
                    , reason :: String
                    , kicker :: String }
             | Topic { content :: String }
             deriving Show

