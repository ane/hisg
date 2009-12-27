-- Hessu - IRC stats generator.
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
common [] = []
common xs = let sorted = qsort xs in sortBy cmp $ group $ sorted

mostCommon [] = []
mostCommon list@(x:xs) = head . common $ list


