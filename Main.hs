module Main where

import Data.List
import Data.Maybe
import System (getArgs)
import System.Console.GetOpt

import Types
import Log
import User
import Misc
import Formatter

main = do
    args <- getArgs
    file <- readFile "battlegroup5.log"
    let decoded = map (fromMaybe (Simple "") . (decode . (++ "\n"))) (lines file)
    putStrLn (intercalate "\n" (map (show) (reverse $ qsort (getUserStats decoded))))
    --usersToHTML (reverse . qsort $ buildUsers decoded) "tiea341"
