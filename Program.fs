namespace WowLogScan

module Main =
  open FParsec
  open WowLogScan
  open Target
  open EventLog
  open ScanUnits

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
    if true then
      printfn ""
      printfn "# WORLD BUFFS (raid prep) ---"
      printfn "# The scoring rules: "
      ScanBuffs.printScoringRules
      printfn ""
      
      let worldBuffsReport = ScanBuffs.scanWorldBuffs (raid, events)
      for wb in worldBuffsReport do
        match wb.Key with
        | TargetType.Player p -> printfn "%A %A" p wb.Value
        | _ -> ()

      // Print CSV
      for wb in worldBuffsReport do
        match wb.Key with
        | TargetType.Player p ->
            let effortScore = EffortScore.scoreWorldBuffs (wb.Value)
            printfn "%s,%d,Worldbuff" p effortScore 
        | _ -> ()

    if false then
      printfn ""
      printfn "# ENCOUNTER START BUFFS ---"
      printfn "# List recognized consumable effects up on the combatants when an encounter starts"
      printfn "# The scoring rules: "
      
      ScanCombatStart.printScoringRules
      printfn ""
      let combatantBuffReport = ScanCombatStart.scan (raid, events)
      for cr in combatantBuffReport do
        ScanCombatStart.printReport(raid, cr)

    if true then
      printfn ""
      printfn "# CONSUMABLES IN COMBAT (raid prep) ---"
      printfn "# List recognized consumable effects used throughout the raid"
      printfn ""
      let consumReport = ScanConsumablesInCombat.scan (raid, events)
      ScanConsumablesInCombat.printReport consumReport
      ScanConsumablesInCombat.printEffortPoints consumReport

    if true then
      printfn ""
      printfn "# ENCHANTED GEAR (raid prep) ---"
      printfn "# Listed are all unique items used by players in the raid, with permanent enchants"
      printfn "# Temporary enchant data like oils, poisons and totem effects is available but not analyzed"
      printfn ""
      let enchantsReport = ScanEnchants.scanEnchants (raid, events)
      for er in enchantsReport do
        printfn "%s" (ScanEnchants.printReport er)

  [<EntryPoint>]
  let main (argv: string []): int =
    let filename = argv.[0]

    match CharParsers.runParserOnFile CombatlogSyntax.combatLogEventList () filename System.Text.Encoding.UTF8 with
    | Success (parsedList, _, _) -> handleParsedList parsedList |> ignore
    | Failure (err, _, _) -> printfn "Fail: %s" err
    0
