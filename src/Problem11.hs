module Problem11 where

import Problem10 (encode,map')

data Item a = Multiple Int a | Single a
    deriving (Show)

encode' :: Eq a => [a] -> [Item a]
encode' xs = map' process $ encode xs
            where process (count, elem) 
                    | count == 1 = Single elem
                    | otherwise = Multiple count elem
