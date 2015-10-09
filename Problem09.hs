--9 Problem 9
--(**) Pack consecutive duplicates of list elements into sublists. If a list
--contains repeated elements they should be placed in separate sublists.

--Example:

--  * (pack '(a a a a b c c a a d e e e e))
--  ((A A A A) (B) (C C) (A A) (D) (E E E E))
--Example in Haskell:

--  *Main> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 
--               'a', 'd', 'e', 'e', 'e', 'e']
--  ["aaaa","b","cc","aa","d","eeee"]

module Problem09 (pack) where

map' :: Eq a => [a] ->[[a]] 
map' [] = [[]]
map' [x] = [[x]]
map' (x:xs) = [[x]] ++ map' xs

pack' :: Eq a => [[a]] -> [[a]]
pack' [[]] = [[]]
pack' [x] = [x]
pack' ((x:xs):[y]:xss)
    | x == y = pack' ([[x,y] ++ xs] ++ xss)
    | otherwise = [(x:xs)] ++ pack' ([y]:xss)

pack :: Eq a => [a] -> [[a]]
pack [] = [[]]
pack x = pack' $ map' x
