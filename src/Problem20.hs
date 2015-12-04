module Problem20 where

removeAt :: [a] -> Int -> [a]
removeAt [] _ = []
removeAt (x:xs) 1 = xs
removeAt (x:xs) nth = [x] ++ removeAt xs (nth -1)
