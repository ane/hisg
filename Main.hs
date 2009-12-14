module Main where

import Data.List
import Text.ParserCombinators.Parsec

import Types
import LogParser

main = do
          file <- readFile "tiea341.log"
          mapM_ (printLine) (lines (file))
       where
          printLine str = case decode (str ++ "\n") of 
                            Just s -> 
                                putStrLn (show s)
                            Nothing -> 
                                putStrLn "Parse error lol"

