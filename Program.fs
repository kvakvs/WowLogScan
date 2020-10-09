namespace WowLogScan

module Main =
  open FParsec
  open WowLogScan
  open EventLog
  open ScanUnits
  open ScanBuffs

  let printUnknownEvents logLines =
    for ev in logLines do
      match ev with
      | CombatLogEvent.NotSupported _ -> printfn "%A" ev
      | _ -> ()


  let oldMain (argv: string []): int =
    printfn "WowLogParser for gear check and buffs..."

    let preprocessedLogLines = Parser.loadAndParseLogLines (argv.[0])

    let eventList =
      Parser.createEventList preprocessedLogLines
    // printUnknownEvents logLines

    let raid = scanUnits preprocessedLogLines

    // Print unitId to player mapping
    // for player in raid.Players do printfn "%+A" player

    let worldBuffs = scanWorldBuffs (raid, eventList)
    for wb in worldBuffs do
      printfn "%+A" wb

    0 // return an integer exit code

  [<EntryPoint>]
  let main (argv: string []): int =
    let filename = argv.[0]
    let lines = System.IO.File.ReadAllLines(filename)
//    match CharParsers.runParserOnString CombatlogSyntax.versionString () "test1" "1.13.2" with
//    | Success (ok, _, _) ->
//        printfn "Success: %A" ok
//    | Failure (err, _, _) ->
//        printfn "Fail: %s" err

    for ln in lines.[1..10] do
      match CharParsers.runParserOnString CombatlogSyntax.combatLogEvent () filename ln with
          | Success (ok, _, _) ->
              printfn "Success: %A" ok
          | Failure (err, _, _) ->
              printfn "Fail: %s" err
    0