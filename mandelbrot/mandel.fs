// Learn more about F# at http://fsharp.net
let (winW,winH) = (System.Console.WindowWidth, System.Console.WindowHeight)

type ViewState = { ulX : float; ulY : float; scaleX : float; scaleY : float }
let initState = { ulX = -2.00 ; ulY = 1.00; scaleX = 0.04; scaleY = 0.1 }

let procCmd key st =
  match key with
    | System.ConsoleKey.LeftArrow ->  { st with ulX = st.ulX - 6.0*st.scaleX }
    | System.ConsoleKey.RightArrow -> { st with ulX = st.ulX + 6.0*st.scaleX }
    | System.ConsoleKey.UpArrow ->    { st with ulY = st.ulY - 4.0*st.scaleY }
    | System.ConsoleKey.DownArrow ->  { st with ulY = st.ulY + 4.0*st.scaleY }
    | System.ConsoleKey.I -> { ulX = st.ulX + st.scaleX*((float)winW/4.0) ; 
                               ulY = st.ulY + st.scaleY*((float)winH/4.0) ;
                               scaleX = st.scaleX / 2.0 ;
                               scaleY = st.scaleY / 2.0 }
    | System.ConsoleKey.O -> let sx = st.scaleX * 2.0
                             let sy = st.scaleY * 2.0
                             { ulX = st.ulX - sx*((float)winW/4.0) ; 
                               ulY = st.ulY - sy*((float)winH/4.0) ;
                               scaleX = sx ; scaleY = sy }
    | System.ConsoleKey.Q -> exit 0 
    | _ -> st

let mandel xp yp = 
    let norm x y = x*x + y*y
    let rec iterate ch x y =
        if ch = ' ' 
        then ch
        else if (norm x y) > 4.0 
             then ch
             else iterate (char (int ch - 1)) (x*x - y*y + xp) (x*y*2.0 + yp)
    iterate '~' xp yp
     
let scene st = 
    let line = Array.create winW ' '
    for curY = 0 to winH-1 do
      System.Console.SetCursorPosition(0, curY)
      for curX = 0 to winW-1 do
        line.[curX]  <- mandel (st.ulX+(float curX)*st.scaleX) (st.ulY+(float curY)*st.scaleY)
      printf "%s" (new System.String(line))

let rec iteration st = 
  scene st
  let ch = System.Console.ReadKey true
  iteration ( procCmd ch.Key st )

[<EntryPoint>]
let main argv = 
    System.Console.Clear()
    iteration initState

