module Problem17 where
import Problem16

split' :: [a] -> Int -> ([a], [a])
split' xs index = (filter' (\x i -> i <= index) xs, 
                   filter' (\x i -> i > index) xs)
