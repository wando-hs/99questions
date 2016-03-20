module Problem32(myGCD) where

myGCD :: Int -> Int -> Int
myGCD _ 1 = 1
myGCD x 0 = x
myGCD x y
    | x < y = myGCD y x
    | otherwise = myGCD y $ mod x y