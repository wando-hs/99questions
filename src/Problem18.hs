module Problem18 where
import Problem16 

slice :: [a] -> Int -> Int -> [a]
slice xs start end = filter' (\x i -> i >= start && i <= end) xs
