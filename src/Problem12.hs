import Problem11 (Item(Single, Multiple))

repeat' :: Int -> a -> [a]
repeat' 1 x = [x]
repeat' quantity x = [x] ++ repeat' (quantity - 1) x 

decodeModified :: [Item a] -> [a]
decodeModified [] = []
decodeModified [(Single x)] = repeat' 1 x
decodeModified [(Multiple quantity x)] = repeat' quantity x
decodeModified (x:xs) = decodeModified [x] ++ decodeModified xs
