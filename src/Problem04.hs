--4 Problem 4
--(*) Find the number of elements of a list.

--Example in Haskell:

--  Prelude> myLength [123, 456, 789]
--  3
--  Prelude> myLength "Hello, world!"
--  13

module Problem04 where

myLength [a] = 1
myLength (x:xs) = 1 + myLength xs
