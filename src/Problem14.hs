module Problem14 where

dupli :: [a] -> [a]
dupli [x] = [x,x]
dupli (x:xs) = [x,x] ++ dupli xs
