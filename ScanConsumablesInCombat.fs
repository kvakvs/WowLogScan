namespace WowLogScan

open System
open WowLogScan.Buffs

module ScanConsumablesInCombat =
  open Microsoft.FSharp.Collections
  open EventLog
  open RaidState
  open System.Collections.Generic
  open WowLogScan.Model.Unit

  type GainedLost =
    | Gained
    | Lost
    | Used

  type ConsumableUseEvent =
    { Type: ConsumableClass
      TypeExplanation: string
      Ability: Ability
      Target: Unit
      GainedOrLost: GainedLost }

  type EncounterReport =
    { Encounter: Option<Encounter>
      EncounterSeq: int
      Consumables: ConsumableUseEvent list }

  let isPlayer (u: Unit): bool =
    match u with
    | Player _ -> true
    | PlayerId _ -> true
    | _ -> false

  let formatEncounter (e: Option<Encounter>): string =
    match e with
    | Some encounter -> sprintf "%A" encounter.Boss
    | None -> "<trash>"

  let printReportRow (c: ConsumableUseEvent) =
    printfn "  %A → %A → %A (%A)" c.Target c.GainedOrLost c.Ability c.Type

  let printReport (allEncounters: EncounterReport list) =
    for er in allEncounters do
      printfn "%s #%d" (formatEncounter er.Encounter) er.EncounterSeq

      for row in er.Consumables do
        printReportRow row

  type PlayerEPScores = Dictionary<string, float>

  let writeKeyOnce (dict: PlayerEPScores, key: string, value: float) =
    if dict.ContainsKey key then () else dict.Add(key, value)

  let replaceKey (dict: PlayerEPScores, key: string, value: float) =
    if dict.ContainsKey key then (dict.Remove key) |> ignore
    dict.Add(key, value)

  let mergeGrades (inputs: PlayerEPScores list): PlayerEPScores =
    let grades = PlayerEPScores()

    for inp in inputs do
      for entry in inp do
        if grades.ContainsKey entry.Key
        then replaceKey (grades, entry.Key, grades.[entry.Key] + entry.Value)
        else grades.Add(entry.Key, entry.Value)

    grades

  let capGradesAt (input: PlayerEPScores, cap: float): PlayerEPScores =
    let result = PlayerEPScores()

    for item in input do
      result.Add(item.Key, Math.Min(item.Value, cap))

    result

  let gradeEncounterEP (e: EncounterReport): PlayerEPScores =
    let strongOffensive = PlayerEPScores()
    let weakOffensive = PlayerEPScores()
    let weakFlask = PlayerEPScores()
    let potentFlask = PlayerEPScores()

    for useEvent in e.Consumables do
      let playerName = Model.Unit.playerName useEvent.Target

      match useEvent.Type with
      | ConsumableClass.PotentFlask when useEvent.GainedOrLost = Gained -> writeKeyOnce (potentFlask, playerName, 1.0)
      | ConsumableClass.WeakFlask -> writeKeyOnce (weakFlask, playerName, 1.0)
      | ConsumableClass.PotentOffensive -> writeKeyOnce (strongOffensive, playerName, 1.0)
      | ConsumableClass.WeakOffensive -> writeKeyOnce (weakOffensive, playerName, 0.5)
      | _ -> ()


    let merged = mergeGrades [ potentFlask; weakFlask; strongOffensive; weakOffensive ]
    capGradesAt(merged, 1.0)

  let gradeAllEncountersEP (allEncounters: EncounterReport list): PlayerEPScores =
    // Filter out trash, and grade every encounter
    let allGrades =
      allEncounters
      |> List.filter (fun e -> e.Encounter.IsSome)
      |> List.map gradeEncounterEP

    for g in allGrades do
      printfn "%A" g

    mergeGrades allGrades

  let printEffortPoints (allEncounters: EncounterReport list) =
    printfn ""
    printfn "## EP Grades"
    printfn ""

    let grades = gradeAllEncountersEP allEncounters

    for g in grades do
      printfn "%s,%d,Consumable use" g.Key (Convert.ToInt32 g.Value)

  let scanEncounter (events: CombatLogEvent list): ConsumableUseEvent list =
    let consumableUses = List<ConsumableUseEvent>()

    for ev in events do
      match ev with
      | CombatLogEvent.Spell sp when let gained = sp.Base.Suffix = SpellSuffix.AuraApplied
                                     let lost = sp.Base.Suffix = SpellSuffix.AuraRemoved

                                     sp.Base.Prefix = SpellPrefix.Spell
                                     && (gained || lost)
                                     && isPlayer sp.Base.Target ->
          let explanation, consumableClass = recognizeAbilityAsConsumable sp.Spell

          match consumableClass with
          | Skip -> ()
          | _other ->
              let isGained = sp.Base.Suffix = SpellSuffix.AuraApplied

              let useType =
                if isGained then GainedLost.Gained else GainedLost.Lost

              consumableUses.Add
                ({ Type = consumableClass
                   TypeExplanation = explanation
                   Ability = sp.Spell
                   Target = sp.Base.Target
                   GainedOrLost = useType })
      | CombatLogEvent.Spell sp when sp.Base.Prefix = SpellPrefix.Spell
                                     && sp.Base.Suffix = SpellSuffix.Energize
                                     && isPlayer sp.Base.Target ->
          match recognizeEnergize sp with
          | Some Skip -> ()
          | Some r ->
              consumableUses.Add
                ({ Type = r
                   TypeExplanation = "Energize"
                   Ability = sp.Spell
                   Target = sp.Base.Target
                   GainedOrLost = GainedLost.Used })
          | None -> ()
      | _ -> ()

    consumableUses |> Seq.toList

  let rec scan' (events: CombatLogEvent list, seq: int, accum: EncounterReport list): EncounterReport list =
    match events with
    | [] -> accum |> List.rev
    | _ ->
        let isNotEncounterEnd (ev: CombatLogEvent) =
          match ev with
          | CombatLogEvent.EncounterEnd _ -> false
          | _ -> true

        let isAnyEncounterEvent (ev: CombatLogEvent) =
          match ev with
          | CombatLogEvent.EncounterStart _ -> true
          | CombatLogEvent.EncounterEnd _ -> true
          | _ -> false

        let combatSection = List.takeWhile isNotEncounterEnd events
        let tail = List.skipWhile isNotEncounterEnd events

        let encounter: Option<Encounter> =
          try
            match List.find isAnyEncounterEvent combatSection with
            | CombatLogEvent.EncounterStart e -> Some(e)
            | CombatLogEvent.EncounterEnd e -> Some(e)
            | _ -> None
          with :? KeyNotFoundException -> None

        eprintfn "Processing buffs for: %A" (if encounter.IsSome then sprintf "%A" encounter.Value.Boss else "None")

        let encounterReport =
          { Encounter = encounter
            EncounterSeq = seq
            Consumables = scanEncounter combatSection }

        scan' (tail |> List.skipWhile isAnyEncounterEvent, seq + 1, encounterReport :: accum)

  let scan (_raid: RaidState, events: CombatLogEvent list): EncounterReport list =
    //    let mutable encounterSeq = 0
    //    let mutable inEncounter: Option<Encounter> = None
    scan' (events, 1, [])
