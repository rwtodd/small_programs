module Main where

import Data.Char(isUpper,isLower,ord,chr)

atbash ch | isUpper ch = chr $ ord 'Z' - ord ch + ord 'A'
          | isLower ch = chr $ ord 'z' - ord ch + ord 'a'
          | otherwise  = ch

main = do 
  ln <- getLine
  putStrLn (map atbash ln)
  if ln /= "quit" then main else return ()
