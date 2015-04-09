{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RecordWildCards        #-}
module Hisg.Analysis.Statistic
       ( Users
       , Behaviour(..)
       , compute
       , identifier
       , behaviours
       , coalesce
       ) where

import           Data.List.Split
import qualified Data.Map            as M
import           Data.Monoid
import           Hisg.Analysis.Event

data Behaviour = Behaviour { lineCount :: Int
                           , wordCount :: Int
                           , kicks     :: Int }
                 deriving Show

instance Monoid Behaviour where
  mempty = Behaviour { lineCount = 0, wordCount = 0, kicks = 0 }
  mappend (Behaviour lc1 wc1 k1) (Behaviour lc2 wc2 k2) = Behaviour (lc1 + lc2) (wc1 + wc2) (k1 + k2)

type Users a = M.Map String a

identifier :: Event -> Maybe String
identifier Kick{..} = Just kicker
identifier Message{..} = Just author
identifier _ = Nothing

compute :: Event -> Behaviour
compute evt =
  let def = Behaviour { lineCount = 0, wordCount = 0, kicks = 0 } in
  case evt of
    Kick{..}    -> def { kicks = 1 }
    Message{..} -> def { lineCount = 1, wordCount = length . splitOn " " $ contents }
    _           -> def


behaviours :: [Event] -> Users Behaviour
behaviours = foldr add M.empty
  where
    add e m = maybe m (\user -> M.insertWith mappend user (compute e) m) (identifier e)

coalesce :: Monoid m => Users m -> Users m -> Users m
coalesce = M.unionWith mappend
