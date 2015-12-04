module Problem16 where

filter'1 :: (a -> Int -> Bool) -> [a] -> Int -> [a]
filter'1 func [] i = []
filter'1 func [x] i = if (func x i) then [x] else []
filter'1 func (x:xs) i = (filter'1 func [x] i) ++ (filter'1 func xs (i + 1))

filter' :: (a -> Int -> Bool) -> [a] -> [a]
filter' func xs = filter'1 func xs 1

dropEvery :: [a] -> Int -> [a]
dropEvery xs nth = filter' (\x i -> (mod i nth) /= 0) xs
