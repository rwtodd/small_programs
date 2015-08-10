// a version in FSharp

let atbash ch =
  let chint = int ch
  match ch with
   | _ when System.Char.IsUpper ch -> char (int 'Z' - chint + int 'A')
   | _ when System.Char.IsLower ch -> char (int 'z' - chint + int 'a')
   | _                             -> ch

[<EntryPoint>]
let main argv =
  let str = if argv.Length > 0 then argv.[0] else "wizard"
  printfn "%s\n%s" str (String.map atbash str)
  0
