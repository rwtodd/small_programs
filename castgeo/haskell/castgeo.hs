-- cast a geomantic shield
import Data.List(transpose, intersperse)
import qualified System.Random as Rnd

type Figure = [Bool]

dispFigs :: Int -> Int -> [Figure] -> [String]  -- display a line of figures
dispFigs sp1 spMid figs = map comb strs 
  where strs     = map (map toStr) (transpose figs)
        comb ln  = concat $ (spaces sp1) : (intersperse (spaces spMid) ln)
        spaces n = take n (repeat ' ')
        toStr bl = if bl then "  *  " else "*   *"

combine :: [Figure] -> [Figure]  -- combine adjacent figures 
combine [] = []
combine (a : b : fs) = (mergeFigs a b) : (combine fs)
 where mergeFigs = zipWith (\a b -> if a == b then False else True) 

rfig :: IO Figure
rfig = sequence $ take 4 $ repeat $ Rnd.getStdRandom (Rnd.random)
   
displine l = (mapM_ putStrLn l) >> (putStrLn "")

main = do
  moms <- sequence $ take 4 (repeat rfig)
  let line1 = reverse (moms ++ (transpose moms))
  let nieces = combine line1 
  let witnesses = combine nieces
  let judge = combine witnesses
  displine $ dispFigs 2 5 line1 
  displine $ dispFigs 7 15 nieces
  displine $ dispFigs 17 35 witnesses
  displine $ dispFigs 37 0 judge
