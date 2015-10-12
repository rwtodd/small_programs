module Main where

pasc = iterate expand [1] 
  where expand lst = let sums = zipWith (+) lst (tail lst)
                     in 1 : sums ++ [1]

main = 
  putStr $ (unlines . (map show) . (take 16)) pasc
