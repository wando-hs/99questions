combinations'1 :: Int -> [a] -> [[a]]
combinations'1 1 xs = map (\x -> [x]) xs
combinations'1 n (x:xs) = map (\ys -> [x] ++ ys) $ combinations'1 (n-1) xs

combinations :: Int -> [a] -> [[a]]
combinations n r@(x:xs) 
    | n >= (length r) = [r]
    | otherwise = list_combination ++ (combinations n xs) 
            where list_combination = combinations'1 n r
