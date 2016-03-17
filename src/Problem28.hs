module Problem28(lsort, lfsort) where

lsort :: [[a]] -> [[a]]
lsort [] = []
lsort (x:xs) = elements (<=) ++ [x] ++ elements (>)
                        where elements cond = lsort $ filter (not . cond size . length) xs
                              size = length x

lfsort :: [[a]] -> [[a]]
lfsort xs = xs