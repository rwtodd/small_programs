import qualified System.Random as Rnd
import System.Exit (exitFailure)
import System.Environment (getArgs)
import qualified Data.Array.IArray as Arr

-- Casting Methods ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
casting proc = sequence $ take 6 $ repeat proc 

addChar ch amt = (toEnum $ amt + fromEnum ch) :: Char

coin = do 
   g <- Rnd.newStdGen
   return $ addChar '6' (sum $ take 3 $ Rnd.randomRs (0,1) g)
 
static = do
   roll <- Rnd.getStdRandom (Rnd.randomR (0,1))
   return $ addChar '7' roll 

stalk = do
   roll <- ( Rnd.getStdRandom (Rnd.randomR (0,15)) ) :: IO Int
   return $ if roll == 0 then '6' else
            if roll <= 5 then '7' else
            if roll <= 12 then '8' else '9' 
              
-- Hexagram Display Functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
decode = foldr dec (0,0,[])
 where
  dec '6' (n1,n2,ls) = (n1*2   , n2*2+1, "  --   --    -->    -------":ls)
  dec '7' (n1,n2,ls) = (n1*2+1 , n2*2+1, "  -------           -------":ls)
  dec '8' (n1,n2,ls) = (n1*2   , n2*2  , "  --   --           --   --":ls)
  dec '9' (n1,n2,ls) = (n1*2+1 , n2*2  , "  -------    -->    --   --":ls)

hexStrs casting =
  lines ++ ("":names)
  where 
   (hn1,hn2,strs) = decode casting
   changes   = hn1 /= hn2 
   lines     = reverse . (if changes then id else (map (take 9))) $ strs 
   names     = (if changes then id else (take 1))
                 [hexNames Arr.! hn1, " - Changing To ->", hexNames Arr.! hn2]

-- Argument Parsing / Main ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
getCasting "-coins"  = casting coin 
getCasting "-static" = casting static
getCasting "-stalks" = casting stalk 
getCasting c         = if (length c == 6) && 
                          (all (\ch -> ch >= '6' && ch <= '9') c)
                       then return c
                       else (mapM_ putStrLn usage) >> exitFailure 
  where usage = [ "Usage: casthex (-coins|-stalks|-static|<casting>)",
                  "  -coins    3-coins method", 
                  "  -stalks   yarrow stalks method",
                  "  -static   a random hexagram", 
                  "  <casting> 6 digits from the set {6,7,8,9]" ]
   
main = do
  args <- getArgs
  c    <- getCasting (head $ args ++ ["-coins"])
  putStrLn $ "Casting: " ++ c
  putStrLn "" 
  mapM_ putStrLn (hexStrs c)

hexNames :: Arr.Array Int String
hexNames = Arr.listArray (0,63) [ 
    "02. K'un -- Earth",
    "24. Fu -- Return",
    "07. Shih -- The Army",
    "19. Lin -- Overseeing",
    "15. Ch'ien -- Humility",
    "36. Ming I -- Concealment of Illumination",
    "46. Sheng -- Rising",
    "11. T'ai -- Tranquility",
    "16. Yu -- Enthusiasm",
    "51. Chen -- Thunder",
    "40. Hsieh -- Liberation",
    "54. Kuei Mei -- Making a Young Girl Marry",
    "62. Hsiao Kuo -- Predominance of the Small",
    "55. Feng -- Richness",
    "32. Heng -- Constancy",
    "34. Ta Chuang -- Great Power",
    "08. Pi -- Accord",
    "03. Chun -- Difficulty",
    "29. K'an -- Mastering Pitfalls",
    "60. Chieh -- Discipline",
    "39. Chien -- Halting",
    "63. Chi Chi -- Settled",
    "48. Ching -- The Well",
    "05. Hsu  -- Waiting",
    "45. Ts'ui -- Gathering",
    "17. Sui -- Following",
    "47. K'un -- Exhaustion",
    "58. Tui -- Joy",
    "31. Hsien -- Sensitivity",
    "49. Ko -- Revolution",
    "28. Ta Kuo -- Excess of the Great",
    "43. Kuai -- Parting",
    "23. Po -- Stripping Away",
    "27. I -- Lower Jaw (Nourishment)",
    "04. Meng -- Darkness",
    "41. Sun -- Reduction",
    "52. Ken -- Mountain",
    "22. Pi -- Adornment",
    "18. Ku -- Degeneration",
    "26. Ta Ch'u -- Nurturance of the Great",
    "35. Chin -- Advance",
    "21. Shih Ho -- Biting Through",
    "64. Wei Chi -- Unsettled",
    "38. K'uei -- Disharmony",
    "56. Lu -- Travel",
    "30. Li -- Fire",
    "50. Ting -- The Cauldron",
    "14. Ta Yu -- Great Possession",
    "20. Kuan -- Observation",
    "42. I -- Increase",
    "59. Huan -- Dispersal",
    "61. Chung Fu -- Faithfulness in the Center",
    "53. Chien -- Gradual Progress",
    "37. Chia Jen -- People in the Home",
    "57. Sun -- Wind",
    "09. Hsiao Ch'u -- Nurturance by the Small",
    "12. Pi -- Obstruction",
    "25. Wu Wang -- Fidelity (No Error)",
    "06. Sung -- Contention",
    "10. Lu -- Treading",
    "33. Tun -- Withdrawal",
    "13. T'ung Je^n -- Sameness with People",
    "44. Kou -- Meeting",
    "01. Chi'en -- Heaven" ]

