import Data.List

combinations'1 :: Int -> [a] -> [[a]]
combinations'1 1 xs = map (\x -> [x]) xs
combinations'1 n (x:xs) = map (\ys -> [x] ++ ys) $ combinations'1 (n-1) xs

combinations :: Int -> [a] -> [[a]]
combinations n r@(x:xs)
    | n >= (length r) = [r]
    | otherwise = list_combination ++ (combinations n xs)
            where list_combination = combinations'1 n r

--groupera'1 :: [Int] -> [a] -> [a] -> [[[a]]]
--groupera'1 xs fixed elements = map (\ ys -> [fixed] ++ ys)
--                                   (groupera xs elements)
--groupera' :: [[a]] -> 

--groupera :: [Int] -> [a] -> [[[a]]]
--groupera [x] elements = [combinations x elements]
--groupera (x:xs) elements = groupera'1 xs ys (elements \\ ys) ++ (combinations x elements)

--grouperinha :: [Int] -> [a] -> [[[a]]]
--grouperinha (x:xs) elements = combinations x elements
                         -- ++ grouperinha xs ys
                              --where ys = combinations x elements

--groupera :: [Int] -> [a] -> [[[a]]]
--groupera (x:xs) elements = grouperinha [x ++ xs] elements 

gpr' :: Eq a => [Int] -> [[a]] -> [a] -> [[[a]]]
gpr' [] fixed elements = [fixed]
gpr' (x:xs) [] elements = gpr' xs (combinations x elements) elements
gpr' c@(x:xs) (f:fs) elements =
    (map (\ ys -> [f] ++ ys) (gpr' xs (combinations x els) els))
    ++
    (gpr' c fs elements)
    where els = elements \\ f





