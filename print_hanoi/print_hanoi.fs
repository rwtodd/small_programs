// fsharp version

let draw_hanoi col1 col2 col3 spacing =
  let spacingstr = String.replicate spacing " "
  let dsize = 2*d + 1
  let colsz = dsize (Array.max [| Array.max col1 ; Array.max col2 ; Array.max col3 |])
  let height = col1.Length + col2.Length + col3.Length
  let draw1 idx (arr:int[]) =
     let offset = height - arr.Length
     let cindx = idx - offset
     let (dchr,dsz) = if cindx >= 0 then ("-",dsize arr.[cindx]) else ("|",1)
     let spaces = String.replicate ((colsz - dsz)/2) " "
     let disk = String.replicate dsz dchr
     printf "%s%s%s%s" spaces disk spaces spacingstr
  let rec each_disk cur = 
     if cur = height
     then ()
     else
        draw1 cur col1 ;  draw1 cur col2 ;  draw1 cur col2 ;  printfn ""
        each_disk (cur+1)
  each_disk 0

[<EntryPoint>]
let main argv =
  draw_hanoi [| 4; 5; 6; 7; |]  [| 2; 3 |]  [| 1 |]  1
  0

