module Problem21 where

insertAt :: a -> [a] -> Int -> [a]
insertAt _ [] _ = []
insertAt x xs 1 = (x:xs)
insertAt x (y:ys) nth = [y] ++ insertAt x ys (nth - 1)
