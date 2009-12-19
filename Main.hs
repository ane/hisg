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
            --mapM_ (printLine) (decoded)
            putStrLn (show (countLines "ibid" (map (fromMaybe (Simple "")) (decoded))))
        where
            printLine s = case s of
                            Just s ->
                                putStrLn (show s)
                            Nothing ->
                                putStrLn "Parse error lol"

