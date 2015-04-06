{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
module Hisg.Analysis.Statistic
       ( User(..)
       , UserStats(..)
       , Statistic(..)
       , compute
       ) where

import Hisg.Analysis.Event
import Data.List.Split

data User = User String UserStats

data UserStats = UserStats { lineCount :: Int
                           , wordCount :: Int
                           , kicks     :: Int }
class Statistic a b where
  identifier :: a -> String
  update :: a -> b -> b

instance Statistic Kick UserStats where
  identifier = kicker
  update Kick {..} u = u { kicks = 1 + kicks u }

instance Statistic Message UserStats where
  identifier = author
  update (Message _ contents) u = u { lineCount = lineCount u + 1, wordCount = wordCount u + w }
    where
      w = length . splitOn " " $ contents

compute :: Statistic a b => a -> b -> b
compute = update
