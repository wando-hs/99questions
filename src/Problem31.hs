module Problem31(isPrime) where

isPriminho :: Int -> Int -> Bool
isPriminho x y 
    | x == y = True
    | otherwise = if x `mod` y == 0 then False else isPriminho x (y + 1)

isPrime :: Int -> Bool
isPrime 1 = True
isPrime x = isPriminho x 2