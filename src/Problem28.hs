module Problem28(lsort, lfsort) where

import Data.List(sortBy)
import Data.Function(on)

lsort :: [[a]] -> [[a]]
lsort [] = []
lsort (x:xs) = elements (<=) ++ [x] ++ elements (>)
                        where elements cond = lsort $ filter (not . cond size . length) xs
                              size = length x

lfsort :: [[a]] -> [[a]]
lfsort xs = concatMap (takeListsWithSize xs) $ sortBy (compare `on` snd) sizesGrouped 
                    where sizes = foldl (\ s current -> s ++ [length current]) [] xs
                          sizesGrouped = foldl (\ group item -> lInsert item group) [] sizes

takeListsWithSize :: [[a]] -> (Int, Int) -> [[a]]
takeListsWithSize [] _ = []
takeListsWithSize (x:xs) s@(size, _)
    | (length x) == size = x : takeListsWithSize xs s
    | otherwise = takeListsWithSize xs s

lInsert :: Int -> [(Int, Int)] -> [(Int, Int)]
lInsert size [] = [(size, 1)]
lInsert size ((s, frequency):xs) 
    | size == s = (s, frequency + 1) : xs
    | otherwise = (s, frequency) : lInsert size xs
