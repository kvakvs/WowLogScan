namespace WowLogScan

module Main =
  open FParsec
  open WowLogScan
  open EventLog
  open ScanUnits
  open ScanBuffs
  open ScanConsumables
  open ScanEnchants

  let printUnknownEvents logLines =
    for ev in logLines do
      match ev with
      | CombatLogEvent.NotSupported _ -> printfn "%A" ev
      | _ -> ()


  let handleParsedList astEvents =
    let events = astEvents |> List.mapi (fun index v -> Parser.createEvent(v, index)) 

    // Parse unit ids and match to player names    
    let raid = scanUnits astEvents

    // Parse world buffs gained/lost for raid prep/contribution
    printfn "--- WORLD BUFFS (raid prep) ---"    
    let worldBuffsReport = scanWorldBuffs (raid, events)
    for wb in worldBuffsReport do
      printfn "%A %A" wb.Key wb.Value
      
    printfn "--- CONSUMABLES (raid prep) ---"
    let consumReport = scanConsumables (raid, events)
    for cr in consumReport do
      printfn "%s" (ScanConsumables.printReport cr)

    printfn "--- ENCHANTED GEAR (raid prep) ---"
    let enchantsReport = scanEnchants (raid, events)
    for er in enchantsReport do
      printfn "%s" (ScanEnchants.printReport er)

  [<EntryPoint>]
  let main (argv: string []): int =
    let filename = argv.[0]
//    let lines = System.IO.File.ReadAllLines(filename)

    match CharParsers.runParserOnFile CombatlogSyntax.combatLogEventList () filename System.Text.Encoding.UTF8 with
    | Success (parsedList, _, _) ->
        handleParsedList parsedList |> ignore
    | Failure (err, _, _) -> printfn "Fail: %s" err
    0
