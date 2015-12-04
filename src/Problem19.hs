module Problem19 where
import Problem18

rotate :: [a] -> Int -> [a]
rotate xs i 
        | i > 0 = (slice xs (i+1) size) ++ slice xs 0 i
        | otherwise = (slice xs (size + i + 1) size) ++ slice xs 0 (size+i)
            where size = length xs
