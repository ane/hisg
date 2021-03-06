-- hisg - IRC stats generator.
--
-- Copyright (c) 2009, 2010 Antoine Kalmbach <antoine dot kalmbach at jyu dot fi>
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--     * Neither the name of the author nor the
--       names of its contributors may be used to endorse or promote products
--       derived from this software without specific prior written permission.
--
-- For further details, see LICENSE.

module Hisg.Misc (
    invqsort,
    common,
    mostCommon,
    cmp,
    chunk,
    crevf
    ) where

import Data.List

invqsort [] = []
invqsort (x:xs) = invqsort (filter (>= x) xs) ++ [x] ++ invqsort (filter (< x) xs)

cmp xs ys | length xs > length ys = LT
          | otherwise = GT

-- | split at regular intervals
chunk :: Int -> [a] -> [[a]]
chunk _ [] = [[]]
chunk n xs = y1 : chunk n y2
  where
    (y1, y2) = splitAt n xs

common :: (Ord a) => [a] -> [[a]]
common [] = []
common xs = let sorted = (reverse . invqsort) xs in sortBy cmp $ group $ sorted

mostCommon [] = []
mostCommon list@(x:xs) = head . common $ list

crevf [] _ = []
crevf _ [] = []
crevf xs yL@(y:ys) = let f x = y in (take 6 $ map f xs) : (crevf xs ys)
