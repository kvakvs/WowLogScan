﻿namespace WowLogScan

module Main =
  open FParsec
  open WowLogScan
  open WowLogScan.Model.Unit
  open EventLog
  open ScanUnits
  open ScanConsumables
  open ScanEnchants

  let printUnknownEvents logLines =
    for ev in logLines do
      match ev with
      | CombatLogEvent.NotSupported _ -> printfn "%A" ev
      | _ -> ()


  let handleParsedList astEvents =
    let events =
      astEvents
      |> List.mapi (fun index v -> Parser.createEvent (v, index))

    // Parse unit ids and match to player names
    let raid = scanUnits astEvents

    // Parse world buffs gained/lost for raid prep/contribution
    printfn ""
    printfn "# WORLD BUFFS (raid prep) ---"
    printfn "# The scoring rules: "
    ScanBuffs.printScoringRules
    printfn ""
    
    let worldBuffsReport = ScanBuffs.scanWorldBuffs (raid, events)
    for wb in worldBuffsReport do
      match wb.Key with
      | Player p ->
          let effortScore = EffortScore.scoreWorldBuffs (wb.Value)
          printfn "%d %A %A" effortScore p wb.Value
      | _ -> ()

    printfn ""
    printfn "# ENCOUNTER BUFFS (raid consumables) ---"
    printfn "# List recognized consumable effects up on the combatants when an encounter starts"
    printfn "# The scoring rules: "
    
    ScanCombatantBuffs.printScoringRules
    printfn ""
    let combatantBuffReport = ScanCombatantBuffs.scanCombatantBuffs (raid, events)
    for cr in combatantBuffReport do
      ScanCombatantBuffs.printReport(raid, cr)

    if true then
      printfn ""
      printfn "# CONSUMABLES (raid prep) ---"
      printfn "# List recognized consumable effects used throughout the raid"
      printfn ""
      let consumReport = scanConsumables (raid, events)
      for cr in consumReport do
        printfn "%s" (ScanConsumables.printReport cr)

    printfn ""
    printfn "# ENCHANTED GEAR (raid prep) ---"
    printfn "# Listed are all unique items used by players in the raid, with permanent enchants"
    printfn "# Temporary enchant data like oils, poisons and totem effects is available but not analyzed"
    printfn ""
    let enchantsReport = scanEnchants (raid, events)
    for er in enchantsReport do
      printfn "%s" (ScanEnchants.printReport er)

  [<EntryPoint>]
  let main (argv: string []): int =
    let filename = argv.[0]
    //    let lines = System.IO.File.ReadAllLines(filename)

    match CharParsers.runParserOnFile CombatlogSyntax.combatLogEventList () filename System.Text.Encoding.UTF8 with
    | Success (parsedList, _, _) -> handleParsedList parsedList |> ignore
    | Failure (err, _, _) -> printfn "Fail: %s" err
    0
