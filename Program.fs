namespace WowLogScan

open WowLogScan.CombatlogType

module Main =
  open FParsec
  open WowLogScan
  open EventLog
  open ScanUnits

  let printUnknownEvents logLines =
    for ev in logLines do
      match ev with
      | CombatLogEvent.NotSupported _ -> printfn "%A" ev
      | _ -> ()


  let handleParsedList astEvents =
    let events = astEvents |> List.mapi (fun index v -> Parser.createEvent(v, index)) 
    
    let raid = scanUnits astEvents
    printfn "%+A" raid
    
//    let worldBuffs = scanWorldBuffs (raid, eventList)
//    for wb in worldBuffs do
//      printfn "%+A" wb


  //  let oldMain (argv: string []): int =
//    printfn "WowLogParser for gear check and buffs..."
//
//    let preprocessedLogLines = Parser.loadAndParseLogLines (argv.[0])
//
//    let eventList =
//      Parser.createEventList preprocessedLogLines
//    // printUnknownEvents logLines
//
//    let raid = scanUnits preprocessedLogLines
//
//    // Print unitId to player mapping
//    // for player in raid.Players do printfn "%+A" player
//
//    let worldBuffs = scanWorldBuffs (raid, eventList)
//    for wb in worldBuffs do
//      printfn "%+A" wb
//
//    0 // return an integer exit code

  [<EntryPoint>]
  let main (argv: string []): int =
    let filename = argv.[0]
//    let lines = System.IO.File.ReadAllLines(filename)

    match CharParsers.runParserOnFile CombatlogSyntax.combatLogEventList () filename System.Text.Encoding.UTF8 with
    | Success (parsedList, _, _) ->
        handleParsedList parsedList |> ignore
    | Failure (err, _, _) -> printfn "Fail: %s" err
    0
