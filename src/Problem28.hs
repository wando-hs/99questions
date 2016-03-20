module Problem28(lsort, lfsort) where

lsort :: [[a]] -> [[a]]
lsort [] = []
lsort (x:xs) = elements (<=) ++ [x] ++ elements (>)
                        where elements cond = lsort $ filter (not . cond size . length) xs
                              size = length x


lfsort :: [[a]] -> [[a]]
lfsort [] = []
lfsort l@(x:xs) = elements (withFrequency (<)) ++ [x] ++ elements (withFrequency (>=))
                        where elements cond = lfsort $ filter cond xs
                              withFrequency pred y = pred (frequency y) (frequency x)
                              frequency el = length $ filter (length el ==) $ map length l

