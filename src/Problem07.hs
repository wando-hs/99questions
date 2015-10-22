--7 Problem 7
--(**) Flatten a nested list structure.

--Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).

--Example:

--  * (my-flatten '(a (b (c d) e)))
--  (A B C D E)
--  Example in Haskell:

--We have to define a new data type, because lists in Haskell are homogeneous.

--  data NestedList a = Elem a | List [NestedList a]

--  *Main> flatten (Elem 5)
--  [5]
--  *Main> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
--  [1,2,3,4,5]
--  *Main> flatten (List [])
--  []
module Problem07 where

flatten :: [[a]] -> [a]
flatten [lista] = lista
flatten (head:tail) = head ++ flatten tail 

data NestedList a = Elem a | List [NestedList a] deriving (Show)

flatten' :: NestedList a -> [a]
flatten' (List []) = [] 
flatten' (Elem el) = [el]