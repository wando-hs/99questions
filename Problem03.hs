--3 Problem 3
--(*) Find the K'th element of a list. The first element in the list is number 1.

--Example:

--  * (element-at '(a b c d e) 3)
--  c

--Example in Haskell:

--  Prelude> elementAt [1,2,3] 2
--  2
--  Prelude> elementAt "haskell" 5
--  'e'



kth :: [a] -> Int -> a
kth (x:xs) n
        | n == 1 = x
        | otherwise = kth xs $ n - 1
