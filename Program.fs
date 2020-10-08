namespace WowLogScan

open WowLogScan
open WowLogScan.Buffs

module Main =
  open EventLog
  open ScanUnits
  open ScanBuffs

  let printUnknownEvents logLines =
    for ev in logLines do
      match ev with
      | CombatLogEvent.NotSupported _ -> printfn "%A" ev
      | _ -> ()


  [<EntryPoint>]
  let main argv =
    printfn "WowLogParser for gear check and buffs..."

    let preprocessedLogLines = Parser.loadAndParseLogLines (argv.[0])

    let eventList =
      Parser.createEventList preprocessedLogLines
    // printUnknownEvents logLines

    let raid = scanUnits preprocessedLogLines

    // Print unitId to player mapping
    // for player in raid.Players do printfn "%+A" player

    let worldBuffs = scanWorldBuffs (raid, eventList)
    for wb in worldBuffs do printfn "%+A" wb

    0 // return an integer exit code
