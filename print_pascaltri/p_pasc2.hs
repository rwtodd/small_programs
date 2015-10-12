module Main where

pasc = [1] : map expand pasc
  where expand lst = 1 : (sums lst) ++ [1]
        sums x  = zipWith (+) x (tail x)  

main = 
  putStr $ (unlines . (map show) . (take 16)) pasc
