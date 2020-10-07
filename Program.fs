namespace WowLogScan

open WowLogScan

module Main =

  open LogEntity

  [<EntryPoint>]
  let main argv =
    printfn "WowLogParser for gear check and buffs..."
    
    for ev in Parser.readLogLines(0, 500000) do
      match ev with
      | Event.NotSupported _ -> printfn "%A" ev
      | _ -> ()
      
    0 // return an integer exit code
