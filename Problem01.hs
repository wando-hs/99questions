--1 Problem 1
--(*) Find the last element of a list.
--(Note that the Lisp transcription of this problem is incorrect.)

--    Example in Haskell:

--    Prelude> myLast [1,2,3,4]
--    4
--    Prelude> myLast ['x','y','z']
--    'z'

module Problem01 (mylast) where

mylast :: [a] -> a
mylast [x] = x
mylast (x:xs) = mylast xs
