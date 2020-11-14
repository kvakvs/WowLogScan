namespace WowLogScan

module ScanConsumablesInCombat =
  open WowLogScan.Model.GearPiece
  open System
  open WowLogScan.Buffs
  open Microsoft.FSharp.Collections
  open EventLog
  open RaidState
  open System.Collections.Generic
  open CombatlogType

  type GainedLost =
    | Gained
    | Lost
    | Enchanted
    | Used
  
  type ConsumableUseEvent =
    { Type: ConsumableClass
      TypeExplanation: string
      Ability: Option<Ability>
      Caster: Unit
      Target: Unit
      GainedOrLost: GainedLost }

  type EncounterReport =
    { Encounter: Option<Encounter>
      EncounterSeq: int
      Consumables: ConsumableUseEvent list }

  type ScoreMap = Dictionary<string, float>

  let isPlayer (u: Unit): bool =
    match u with
    | TargetType.Player _ -> true
    | TargetType.PlayerId _ -> true
    | _ -> false

  let formatEncounter (e: Option<Encounter>): string =
    match e with
    | Some encounter -> sprintf "%A" encounter.Boss
    | Option.None -> "<trash>"

  let printReportRow (c: ConsumableUseEvent) =
    let explainAbility(a: Option<Ability>): string =
      match a with
      | Some(Ability.Spell(spellId, name)) -> sprintf "%A %s (%s)" spellId name c.TypeExplanation
      | Some a -> sprintf "%A (%s)" a c.TypeExplanation
      | Option.None -> c.TypeExplanation
      
    printfn "  %A → %A → %s (%A)" c.Caster c.GainedOrLost (explainAbility c.Ability) c.Type

  let printReport (allEncounters: EncounterReport list) =
    for er in allEncounters do
      printfn "%s #%d" (formatEncounter er.Encounter) er.EncounterSeq

      for row in er.Consumables do
        printReportRow row

  let writeKeyOnce (dict: ScoreMap, key: string, value: float) =
    if dict.ContainsKey key then () else dict.Add(key, value)

  let replaceKey (dict: ScoreMap, key: string, value: float) =
    if dict.ContainsKey key then (dict.Remove key) |> ignore
    dict.Add(key, value)

  let addScores (inputs: ScoreMap list): ScoreMap =
    let grades = ScoreMap()

    for inp in inputs do
      for entry in inp do
        if grades.ContainsKey entry.Key
        then replaceKey (grades, entry.Key, grades.[entry.Key] + entry.Value)
        else grades.Add(entry.Key, entry.Value)

    grades

  let clampAllValuesAt (input: ScoreMap, cap: float): ScoreMap =
    let result = ScoreMap()

    for item in input do
      result.Add(item.Key, Math.Min(item.Value, cap))

    result

  let gradeEncounterEP (e: EncounterReport): ScoreMap =
    let potentOffensive = ScoreMap()
    let weakOffensive = ScoreMap()
    let potentDefensive = ScoreMap()
    let weakDefensive = ScoreMap()
    let weakFlask = ScoreMap()
    let potentFlask = ScoreMap()
    let potion = ScoreMap()
    let food = ScoreMap()

    for useEvent in e.Consumables do
      let playerName =
        if isPlayer useEvent.Caster then Target.playerName useEvent.Caster else Target.playerName useEvent.Target

      match useEvent.Type with
      | ConsumableClass.PotentFlask when useEvent.GainedOrLost = Gained -> writeKeyOnce (potentFlask, playerName, 1.0)
      | ConsumableClass.WeakFlask -> writeKeyOnce (weakFlask, playerName, 1.0)
      | ConsumableClass.PotentOffensive -> writeKeyOnce (potentOffensive, playerName, 1.0)
      | ConsumableClass.WeakOffensive -> writeKeyOnce (weakOffensive, playerName, 0.5)
      | ConsumableClass.PotentDefensive -> writeKeyOnce (potentDefensive, playerName, 0.5)
      | ConsumableClass.WeakDefensive -> writeKeyOnce (weakDefensive, playerName, 0.25)
      | ConsumableClass.Potion -> writeKeyOnce (potion, playerName, 0.5)
      | ConsumableClass.Food -> writeKeyOnce (food, playerName, 0.25)
      | _ -> ()


    let merged =
      addScores [ potentFlask
                  weakFlask
                  potentOffensive
                  weakOffensive
                  potentDefensive
                  weakDefensive
                  potion
                  food ]

    clampAllValuesAt (merged, 1.0)

  let gradeAllEncountersEP (allEncounters: EncounterReport list): ScoreMap =
    // Filter out trash, and grade every encounter
    let encounters =
      allEncounters
      |> List.filter (fun e -> e.Encounter.IsSome)

    let grades = encounters |> List.map gradeEncounterEP

    let encounterGradePairs = List.zip encounters grades

    for (e, g) in encounterGradePairs do
      printfn "Details for %A:" e.Encounter.Value.Boss

      for i in g do
        printf "%A; " i

      printfn ""

    addScores grades

  let printEffortPoints (allEncounters: EncounterReport list) =
    printfn ""
    printfn "## EP Grades"
    printfn ""

    let grades = gradeAllEncountersEP allEncounters

    for g in grades do
      printfn "%s,%d,Consumable use" g.Key (Convert.ToInt32 g.Value)

  let scanEncounter (raid: RaidState,
                     events: CombatLogEvent list): ConsumableUseEvent list =
    let consumableUses = List<ConsumableUseEvent>()

    let recognizeSpellAndStoreUseEvent (sp: TargetedSpell) =
      let explanation, consumableClass = recognizeAbilityAsConsumable sp.Spell

      match consumableClass with
      | Skip -> ()
      | _other ->
          let useType =
            match sp.Base.Suffix with
            | SpellSuffix.AuraApplied -> Gained
            | SpellSuffix.AuraRemoved -> Lost
            | SpellSuffix.CastSuccess -> Used
            | _ -> failwithf "Expected only spells with aura gained/lost or instant cast: %A" sp

          consumableUses.Add
            ({ Type = consumableClass
               TypeExplanation = explanation
               Ability = Some sp.Spell
               Caster = sp.Base.Caster
               Target = sp.Base.Target
               GainedOrLost = useType })

    let recognizeOneTemporaryEnchantment (player: TargetType.Unit, e: Enchantment) =
      let consClass, explanation = recognizeEnchantment e

      match consClass with
      | ConsumableClass.Skip -> ()
      | _ ->
          consumableUses.Add
            ({ Type = consClass
               Ability = Option.None
               TypeExplanation = explanation
               Caster = player
               Target = player
               GainedOrLost = Enchanted })

    let rec recognizeTemporaryEnchantments (player: TargetType.Unit, gearList: GearPiece list) =
      match gearList with
      | [] -> ()
      | g :: tail when g.Enchants.Length = 3 ->
          for e in g.Enchants do
            recognizeOneTemporaryEnchantment (player, e)
          recognizeTemporaryEnchantments (player, tail)
      | _ :: tail -> recognizeTemporaryEnchantments (player, tail)

    for ev in events do
      match ev with
      | CombatLogEvent.CombatantInfo ci ->
          // For COMBATANT_INFO we can extract temporary enchants up on players' weapons
          let player = resolvePlayer(raid, ci.Player)
          recognizeTemporaryEnchantments (player, ci.Equipment)
      | CombatLogEvent.Spell sp when sp.Base.Suffix = SpellSuffix.CastSuccess
                                     && isPlayer sp.Base.Caster ->
          // For SPELL_* we can track usage of consumables
          recognizeSpellAndStoreUseEvent sp
      | _ -> ()

    consumableUses |> Seq.toList

  let rec scan' (raid: RaidState, events: CombatLogEvent list, seq: int, accum: EncounterReport list)
                : EncounterReport list =
    let isNotEncounterEnd (ev: CombatLogEvent) =
      match ev with
      | CombatLogEvent.EncounterEnd _ -> false
      | _ -> true

    let isAnyEncounterEvent (ev: CombatLogEvent) =
      match ev with
      | CombatLogEvent.EncounterStart _ -> true
      | CombatLogEvent.EncounterEnd _ -> true
      | _ -> false

    match events with
    | [] -> accum |> List.rev
    | _ ->
        let combatSection = List.takeWhile isNotEncounterEnd events
        let tail = List.skipWhile isNotEncounterEnd events

        let encounter: Option<Encounter> =
          try
            match List.find isAnyEncounterEvent combatSection with
            | CombatLogEvent.EncounterStart e -> Some(e)
            | CombatLogEvent.EncounterEnd e -> Some(e)
            | _ -> Option.None
          with :? KeyNotFoundException -> Option.None

        eprintfn "Processing buffs for: %A" (if encounter.IsSome then sprintf "%A" encounter.Value.Boss else "None")

        let encounterReport =
          { Encounter = encounter
            EncounterSeq = seq
            Consumables = scanEncounter(raid, combatSection) }

        scan' (raid, tail |> List.skipWhile isAnyEncounterEvent, seq + 1, encounterReport :: accum)

  let scan (raid: RaidState, events: CombatLogEvent list): EncounterReport list =
    //    let mutable encounterSeq = 0
    //    let mutable inEncounter: Option<Encounter> = None
    scan' (raid, events, 1, [])
