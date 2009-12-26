module Misc (
    qsort,
    common,
    mostCommon,
    cmp
    ) where

import Data.List

qsort [] = []
qsort (x:xs) = qsort (filter (< x) xs) ++ [x] ++ qsort (filter (>= x) xs)

cmp xs ys | length xs > length ys = LT
          | otherwise = GT

common :: (Ord a) => [a] -> [[a]]
common [] = error "Empty list"
common xs = let sorted = qsort xs in sortBy cmp $ group $ sorted

mostCommon [] = error "Empty list!"
mostCommon list@(x:xs) = head . common $ list


