module Problem24 where

import Problem23

diff_select :: Int -> Int -> IO [Int]
diff_select n maxBound = rnd_select [1..maxBound] n 
