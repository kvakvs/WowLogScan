namespace WowLogScan

module ScanCombatStart =
  open WowLogScan
  open WowLogScan.Buffs
  open WowLogScan.RaidState
  open EventLog
  open CombatlogType

  let printScoringRules =
    printfn "## Non-trash encounters can grant fractions of EP up to the maximum of 1 EP"
    printfn "## depending on the quality of self-buffs (elixirs, food, extras)"

  // For each aura spellid on player, contains recognized name and consumable class
  type AuraReport =
    { Spell: SpellId
      Explanation: string
      Class: ConsumableClass }

  // For a player store list of buffs on that player, and recognized consumable classes
  type UnitAuras =
    { Player: Unit
      Auras: AuraReport list }

  type EncounterReportItem = { Boss: Unit; Players: UnitAuras list }

  let processCombatStart (encounter: Encounter, events: CombatLogEvent list): EncounterReportItem * CombatLogEvent list =
    let isCombatantInfo (ev: CombatLogEvent): bool =
      match ev with
      | CombatLogEvent.CombatantInfo _ -> true
      | _ -> false

    // Partition the list: Take all COMBATANT_INFO, and take the rest (to become the Tail)
    let combatants: CombatLogEvent list = List.takeWhile isCombatantInfo events
    let tail: CombatLogEvent list = List.skipWhile isCombatantInfo events

    let recognizeAuraAndReport (aura: SpellId): AuraReport =
      let buffName, buffClass = recognizeAura aura

      { Spell = aura
        Class = buffClass
        Explanation = buffName }

    let processCombatant (ev: CombatLogEvent): UnitAuras =
      match ev with
      | CombatLogEvent.CombatantInfo ci ->
          { Player = ci.Player
            Auras =
              ci.Auras
              |> List.map recognizeAuraAndReport
              |> List.filter (fun a -> a.Class <> Skip) }
      | _ -> failwithf "COMBATANT_INFO expected, instead got %A" ev

    let players = List.map processCombatant combatants

    ({ Boss = encounter.Boss
       Players = List.filter (fun p -> p.Auras.Length > 0) players },
     tail)

  let rec forAllEventLog (events: CombatLogEvent list, accum: EncounterReportItem list): EncounterReportItem list =
    match events with
    | [] -> accum
    | CombatLogEvent.EncounterStart encounter :: tail ->
        // Found ENCOUNTER_START
        // Consume following COMBATANT_INFO's to produce an encounter report item
        // and spit out the remaining event list tail
        let encReportItem, tail2 = processCombatStart (encounter, tail)
        forAllEventLog (tail2, encReportItem :: accum)
    | _other :: tail ->
        // Other events just skip
        forAllEventLog (tail, accum) // do nothing, continue

  let scan (_raid: RaidState, events: CombatLogEvent list): EncounterReportItem list =
    forAllEventLog (events, []) |> List.rev

  let printReport (raid: RaidState, item: EncounterReportItem) =
    printfn "%A" item.Boss

    for i in item.Players do
      printfn "> %A" (RaidState.resolvePlayer (raid, i.Player))

      for a in i.Auras do
        printfn "--> %A %A" a.Explanation a.Class
