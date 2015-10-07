module Main where

gen_p width cur loc = cur : let nxt = loc + 1
                            in gen_p width ((width-loc)*cur `div` nxt) nxt

pasc n = map line [ 0..(n-1) ]
  where line width = take (width+1) (gen_p width 1 0)

main = do
  mapM_ (putStrLn . show) (pasc 16)

