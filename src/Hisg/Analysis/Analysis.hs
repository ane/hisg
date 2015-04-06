{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Analysis where

import Data.List.Split

data User = User String UserStats

data UserStats = UserStats { lineCount :: Int
                           , wordCount :: Int
                           , kicks :: Int }

data Message = Message { author :: String
                     , contents :: String }
               
data Kick = Kick { target :: String
                 , reason :: String
                 , kicker :: String }

class Statistic a where
  identifier :: a -> String
  update :: a -> UserStats -> UserStats

instance Statistic Kick where
  identifier = kicker
  update Kick {..} u = u { kicks = 1 + kicks u }

instance Statistic Message where
  identifier = author
  update (Message _ contents) u = u { lineCount = lineCount u + 1, wordCount = wordCount u + w }
    where
      w = length . splitOn " " $ contents

  
