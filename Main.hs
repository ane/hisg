module Main where

import Data.List
import Data.Maybe
import Text.ParserCombinators.Parsec

import Types
import LogParser
import Analyzer

main = do
    file <- readFile "tiea341.log"
    let decoded = map (\s -> decode (s ++ "\n")) (lines file)
    putStr "Enter nickname: "
    nick <- getLine
    putStrLn (show (runAnalyzer (countLines nick) (map (fromMaybe (Simple "")) decoded)))
    putStrLn (show (runAnalyzer (countWords nick) (map (fromMaybe (Simple "")) decoded)))
    putStrLn (show (runAnalyzer (countEvents nick "JOIN") (map (fromMaybe (Simple "")) decoded)))
    putStrLn (show (runAnalyzer (countEvents nick "") (map (fromMaybe (Simple "")) decoded)))
  where
    printLine s = case s of
      Just s  -> putStrLn (show s)
      Nothing -> putStrLn "Parse error lol"

