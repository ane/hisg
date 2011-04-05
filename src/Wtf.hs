-- | Sums the elements of two lists together, i.e.
-- sumList [1,2] [3,4] = [1+3, 2+4]
sumList :: (Num a) => [a] -> [a] -> [a]
sumList xs [] = xs
sumList [] ys = ys
sumList (x:xs) (y:ys) = [x+y] ++ sumList xs ys


