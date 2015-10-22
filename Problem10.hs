module Problem10 (encode, map') where

import Problem09 (pack)

count :: [a] -> Int
count [] = 0
count (x:xs) = 1 + count xs

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = (f x) : map' f xs

encode :: Eq a => [a] -> [(Int,a)]
encode xs = map' (\(y:ys) -> ((count (y:ys), y))) $ pack xs
