module Problem22 where

range :: Int -> Int -> [Int]
range n1 n2
  | n1 > n2 = [n1] ++ range (n1 -1) n2
  | n1 < n2 = [n1] ++ range (n1 +1) n2
  | otherwise = [n2]
