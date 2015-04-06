{-# LANGUAGE GADTs           #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Hisg.Analysis.Event
       ( Kick(..)
       , Message(..)
       , Event(..)
       ) where

data Message = Message { author :: String
                     , contents :: String }

data Kick = Kick { target :: String
                 , reason :: String
                 , kicker :: String }
            
data Event a where
  Msg  :: Message -> Event Message
  Boot :: Kick -> Event Kick

