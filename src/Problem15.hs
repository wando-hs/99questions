module Problem15 where

replicate' :: [a] -> Int -> [a]
replicate' [x] 1 = [x]
replicate' [x] n = [x] ++ replicate' [x] (n - 1)
replicate' (x:xs) n = replicate' [x] n ++ replicate' xs n
