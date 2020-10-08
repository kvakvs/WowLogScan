namespace WowLogScan

open WowLogScan

module Main =
  open EventLog
  open ScanUnits
  
  let printUnknownEvents logLines =
    for ev in logLines do
      match ev with
      | CombatLogEvent.NotSupported _ -> printfn "%A" ev
      | _ -> ()

  
  [<EntryPoint>]
  let main argv =
    printfn "WowLogParser for gear check and buffs..."

    let preprocessedLogLines = Parser.loadAndParseLogLines(argv.[0])
    let eventList = Parser.createEventList preprocessedLogLines
    // printUnknownEvents logLines    

    let raid = scanUnits preprocessedLogLines
    
    for player in raid.Players do
      printfn "%+A" player   
      
    0 // return an integer exit code
