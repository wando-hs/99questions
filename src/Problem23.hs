module Problem23 where

import System.Random
import Problem20 

rnd_select' :: RandomGen g => g -> [a] -> Int -> [a]
rnd_select' seed xs n 
    | n == 1 = [xs !! index]
    | otherwise = [xs !! index] ++ rnd_select' newSeed (removeAt xs (index + 1)) (n-1)
        where index    = fst t_uple
              newSeed  = snd t_uple
              t_uple   = randomR (0, (length xs)-1) seed

rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = do
    seed <- newStdGen
    return $ rnd_select' seed xs n


