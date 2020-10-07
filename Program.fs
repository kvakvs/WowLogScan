namespace WowLogScan

open WowLogScan

module Main =
  open EventLog
  open ScanUnits
  
  let printUnknownEvents logLines =
    for ev in logLines do
      match ev with
      | Event.NotSupported _ -> printfn "%A" ev
      | _ -> ()

  
  [<EntryPoint>]
  let main argv =
    printfn "WowLogParser for gear check and buffs..."

    let logLines = Parser.readLogLines(argv.[0])
    // printUnknownEvents logLines    

    let raid = scanUnits logLines
    
    printfn "%A" raid   
      
    0 // return an integer exit code
