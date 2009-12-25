module Main where

import Data.List
import Data.Maybe
import System (getArgs)

import Types
import LogParser
import User
import Misc
import Formatter

main = do
    args <- getArgs
    file <- readFile (head args)
    let decoded = map (fromMaybe (Simple "")) (map (\s -> decode (s ++ "\n")) (lines file))
    --putStrLn (intercalate "\n" (map (show) (reverse $ qsort (buildUsers decoded))))
    usersToHTML (reverse . qsort $ buildUsers decoded) "tiea341"
