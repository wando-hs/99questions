lsort :: [[a]] -> [[a]]
lsort [] = []
lsort (x:xs) = elements (<) ++ [x] ++ elements (>=)
                        where elements cond = sorte $ filter (not . cond size . length) xs
                              size = length x