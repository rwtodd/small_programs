module Main where

import qualified System.IO as IO
import Data.Complex
import Data.Char (chr,ord)

data Scene = Scene { scaleX :: Double, scaleY :: Double, ulX :: Double, ulY :: Double }

-- ideally get real console support at some point...
lenX = 72 
lenY = 20
clrScr = mapM_ (\_ -> putStrLn "") [1..lenY]

-- add more commands soon
parseCmd 'l' scn = scn { ulX = (ulX scn) - 6.0*(scaleX scn) }
parseCmd 'r' scn = scn { ulX = (ulX scn) + 6.0*(scaleX scn) }
parseCmd 'u' scn = scn { ulY = (ulY scn) - 3.0*(scaleY scn) }
parseCmd 'd' scn = scn { ulY = (ulY scn) + 3.0*(scaleY scn) }
parseCmd 'i' scn = Scene { ulX = (ulX scn) + (scaleX scn)*(fromIntegral lenX)/4.0,
                           ulY = (ulY scn) + (scaleY scn)*(fromIntegral lenY)/4.0,
                           scaleX = (scaleX scn) / 2.0,
                           scaleY = (scaleY scn) / 2.0  }
parseCmd 'o' scn = let newScX = (scaleX scn)*2.0
                       newScY = (scaleY scn)*2.0
                   in Scene { scaleX = newScX,
                              scaleY = newScY,
                              ulX = (ulX scn) - newScX*(fromIntegral lenX)/4.0,
                              ulY = (ulY scn) - newScY*(fromIntegral lenY)/4.0 }
parseCmd  _  scn = scn 

-- *************************************
-- the ASCII code for each point:
-- * Declare an infinite series of mandelbrot 
-- * iterations, and then count how many 
-- * are calculated until the norm get high, 
-- * or we get to the maximum iterations.
-- *************************************
mseries loc = iterate (\pt -> pt * pt + loc) loc
mchar lst = chr $ ord '~' - length lst
lowNorm x = (rp*rp + ip*ip) < 4.0
  where rp = realPart x
        ip = imagPart x
maxRange = ord '~' - ord ' '
mpoint =  mchar . (takeWhile lowNorm) . (take maxRange) . mseries

-- *************************************
-- to draw the scene, map the mpoint to a 
-- list of list of points, which will create
-- a list of [Char]
-- *************************************
mline = map mpoint 
points scn = let xcoords = take lenX $ iterate (\c -> c + (scaleX scn))  (ulX scn) 
                 ycoords = take lenY $ iterate (\c -> c + (scaleY scn))  (ulY scn)
             in [ [ x :+ y | x <- xcoords ] | y <- ycoords ]
drawScene scn = do 
  clrScr
  mapM_ putStrLn $  map mline (points scn) 
  putStrLn "(l)eft, (r)ight, (u)p, (d)own   Zoom (i)n or (o)ut   (q)uit"
  putStrLn ""

-- simple: draw the scene, parse a command, loop!
mainLoop scn = do
  drawScene scn
  cmd <- getChar
  if cmd /= 'q' 
  then mainLoop $ parseCmd cmd scn
  else return () 

-- just set up line buffering and get us into the main loop
main = do
  IO.hSetBuffering IO.stdin IO.NoBuffering
  mainLoop $ Scene 0.04 0.1 (-2.0) (-1.0) 
