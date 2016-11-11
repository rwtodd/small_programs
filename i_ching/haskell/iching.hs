-- vim: set filetype=haskell :
module Main where

import Control.Monad (liftM)
import qualified Data.Array.IArray as Arr
import Data.Bits (testBit)
import qualified Data.Tuple as Tuple
import qualified System.IO as IO

pow2 = 1 : map (*2) pow2  -- helper function, powers of 2

boolnum False = 0        -- helper function, bool case analysis
boolnum True  = 1 

-- ------------------------------------------------------------
-- arrange to get from the hex number to the hexagram, and back
-- ------------------------------------------------------------
num2hex' :: Arr.Array Int Int
num2hex' = Arr.listArray (1,64) [ 
    63 , 0 , 17 , 34 , 23 , 58 , 2 , 16 , 55 , 59 , 7 ,
    56 , 61 , 47 , 4 , 8 , 25 , 38 , 3 , 48 , 41 , 37 ,
    32 , 1 , 57 , 39 , 33 , 30 , 18 , 45 , 28 , 14 , 60 ,
    15 , 40 , 5 , 53 , 43 , 20 , 10 , 35 , 49 , 31 , 62 ,
    24 , 6 , 26 , 22 , 29 , 46 , 9 , 36 , 52 , 11 , 13 ,
    44 , 54 , 27 , 50 , 19 , 51 , 12 , 21 , 42 
    ]
hex2num' :: Arr.Array Int Int 
hex2num' = Arr.array (0,63) (map Tuple.swap $ Arr.assocs num2hex')

num2hex = (num2hex' Arr.!)
hex2num = (hex2num' Arr.!)

-- ------------------------------------------------------------
-- make an ADT for our hex...
-- ------------------------------------------------------------
data Hexagram = Hexagram [Int] Int

hex_lines (Hexagram lines _) = lines 
hex_king_wen (Hexagram _ idx) = idx

make_hex_lines lines = Hexagram lines (hex2num the_sum) 
   where the_sum = sum $ zipWith (*) lines pow2
make_hex_wen idx = Hexagram lst idx
  where lst = map (boolnum . (testBit (num2hex idx))) [0..5]


-- -------------------------------------------------------------
-- Helpers derived from our ADT
-- -------------------------------------------------------------

hex_pred hex = make_hex_wen idx'
  where idx  = hex_king_wen hex
        idx' = if (idx > 1) then idx - 1 else 64
hex_succ hex = make_hex_wen idx'
  where idx  = hex_king_wen hex
        idx' = if (idx < 64) then idx + 1 else 1 

hex_change hex which = make_hex_lines $ change' (hex_lines hex) which
  where change' lst 1 = (1 - (head lst)):(tail lst)
        change' lst n = (head lst):(change' (tail lst) (n - 1))

hex_invert hex = make_hex_lines $ map (\x -> 1 - x) (hex_lines hex)

hex_inner hex = make_hex_lines $ lower ++ upper
  where lower = (take 3 . drop 1) orig
        upper = (take 3 . drop 2) orig
        orig  = hex_lines hex

-- ------------------------------------------------------------
-- know the names of the 8 trigrams
-- ------------------------------------------------------------
trigram_names :: Arr.Array Int String
trigram_names = Arr.listArray (0,7) [
  "K'UN / RECEPTIVE / EARTH      ",
  "CHEN / AROUSING / THUNDER     ",
  "K'AN / ABYSMAL / WATER        ",
  "TUI / JOYOUS / LAKE           ",
  "KEN / KEEPING STILL / MOUNTAIN",
  "LI / CLINGING / FIRE          ",
  "SUN / GENTLE / WIND           ",
  "CH'IEN / CREATIVE /HEAVEN     "
  ]
tri2name lines = trigram_names Arr.! total
 where total = sum $ zipWith (*) lines pow2 

upper_tri hex = tri2name (drop 3 (hex_lines hex))
lower_tri hex = tri2name (take 3 (hex_lines hex))

-- ------------------------------------------------------------
-- read in the description data
-- ------------------------------------------------------------
by3s []  = []
by3s lst = (take 3 lst) : by3s (drop 3 lst)

read_descs :: IO (Arr.Array Int [String])
read_descs = do
   descs <-  liftM (by3s . lines) $ readFile "iching.txt"
   return $ Arr.listArray (1,64) descs


-- ------------------------------------------------------------
-- provide functions to control the terminal
-- ------------------------------------------------------------
page = putStr "\ESC[H\ESC[2J"
clear_line = putStr "\ESC[K"
at x y = do
  putStr "\ESC["
  putStr (show (y+1))
  putStr ";"
  putStr (show (x+1))
  putStr "H"

cookedInput = do
  IO.hSetBuffering IO.stdin IO.LineBuffering
  IO.hSetEcho IO.stdin True

rawInput = do
  IO.hSetBuffering IO.stdin IO.NoBuffering
  IO.hSetEcho IO.stdin False


-- ------------------------------------------------------------
-- UI display stuff 
-- ------------------------------------------------------------
drawnlines hex = map draw (hex_lines hex)
  where
     draw 1  = "############"
     draw _  = "#####  #####"


disp_lines hex = mapM_ disp_at_y (zip (drawnlines hex) [12,10..]) 
  where disp_at_y (ln,y) = (at 2 y) >> (putStr ln)

disp_desc [l1,l2,l3] = do
   at 2 14 ; clear_line ; putStr l1
   at 2 16 ; clear_line ; putStr l2
   at 2 17 ; clear_line ; putStr l3   

disp_trigrams hex = do
   at 16 4  ; putStr $ upper_tri hex
   at 16 10 ; putStr $ lower_tri hex 

disphex :: (Arr.Array Int [String]) -> Hexagram -> IO ()
disphex descs hex =
  do
   disp_lines hex
   disp_trigrams hex
   disp_desc $ descs Arr.! (hex_king_wen hex) 

-- control stuff
controls = putStr "(n)ext/(p)rev (f)orwd/(b)ack (g)oto (c)hange \
                  \(i)nner  in(v)ert (q)uit"


-- ------------------------------------------------------------
-- The forward and back mechanism 
-- (just like the forward and back buttons on your web browser)
-- ------------------------------------------------------------
type Pos = ([Hexagram],[Hexagram])   -- (BackList,ForwardList)

make_pos :: Hexagram -> Pos
make_pos hex = ([hex],[])

pos_top :: Pos -> Hexagram
pos_top  (back,_) = head back

pos_push :: Pos -> Hexagram -> Pos
pos_push (back,_) next = (next:back,[])

pos_back :: Pos -> Pos
pos_back ( (b1:b2:rest), forw ) = ( (b2:rest), (b1:forw) )
pos_back other = other

pos_forward :: Pos -> Pos
pos_forward ( back, (f1:rest) ) = ( (f1:back), rest )
pos_forward other = other

-- ------------------------------------------------------------
-- main loop and user input
-- ------------------------------------------------------------
char_prompt words = do
  at 1 23
  clear_line
  putStr words
  IO.hFlush IO.stdout
  c <- IO.getChar
  IO.getLine -- FIXME windows workaround
  return c

num_prompt :: String -> IO (Int)
num_prompt words = do
  at 1 23
  putStr words
  IO.hFlush IO.stdout
  liftM read IO.getLine

main_loop descs = 
  let loop' pos = do
      let top = pos_top pos
      disphex descs top
      c <- char_prompt "Command? "
      case c of
        'n' -> loop' (pos_push pos (hex_succ top))
        'p' -> loop' (pos_push pos (hex_pred top))
        'g' -> do
                nxt <- num_prompt "Go to which hex? "
                loop' (pos_push pos (make_hex_wen nxt))
        'c' -> do
                chg <- num_prompt "Change which line? "
                loop' (pos_push pos (hex_change top chg))
        'f' -> loop' (pos_forward pos)
        'b' -> loop' (pos_back pos)
        'v' -> loop' (pos_push pos (hex_invert top))
        'i' -> loop' (pos_push pos (hex_inner top))
        'q' -> return ()
        _   -> loop' pos 
  in loop' (make_pos (make_hex_wen 1))
  
main = do
  putStrLn "Reading in data..." 
  descArr <- read_descs
  rawInput
  page ; at 1 20 ; controls
  main_loop descArr 
  page ; at 1 1 
  putStrLn "Goodbye!"
  cookedInput

