namespace WowLogScan

module ScanConsumables =
  open Microsoft.FSharp.Collections
  open WowLogScan.Buffs
  open EventLog
  open RaidState
  open System.Collections.Generic
  open WowLogScan.Model.Unit

  type GainedLost =
    | Gained
    | Lost
    | Used

  type ConsumableReportItem =
    { Encounter: Option<Encounter>
      EncounterSeq: int
      Type: ConsumableClass
      TypeExplanation: string
      Ability: Ability
      Target: Unit
      Use: GainedLost }

  let isPlayer (u: Unit): bool =
    match u with
    | Player _ -> true
    | PlayerId _ -> true
    | _ -> false

  let formatEncounter (e: Option<Encounter>): string =
    match e with
    | Some encounter -> sprintf "%A" encounter.Boss
    | None -> "<trash>"

  let printReport (c: ConsumableReportItem): string =
    sprintf "%s encounter %d: %A %A %A" (formatEncounter c.Encounter) c.EncounterSeq c.Target c.Ability c.Type

  let scanConsumables (_raid: RaidState, events: CombatLogEvent list): ConsumableReportItem list =
    let mutable encounterSeq = 0
    let mutable inEncounter: Option<Encounter> = None
    let result = List<ConsumableReportItem>()

    for ev in events do
      match ev with
      | CombatLogEvent.Spell sp when let gained = sp.Base.Suffix = SpellSuffix.AuraApplied
                                     let lost = sp.Base.Suffix = SpellSuffix.AuraRemoved

                                     sp.Base.Prefix = SpellPrefix.Spell
                                     && (gained || lost)
                                     && isPlayer sp.Base.Target ->
          let explanation, consumableClass =
            Buffs.recognizeAbilityAsConsumable sp.Spell

          match consumableClass with
          | Skip -> ()
          | _other ->
              let isGained = sp.Base.Suffix = SpellSuffix.AuraApplied

              let useType =
                if isGained then GainedLost.Gained else GainedLost.Lost

              result.Add
                ({ EncounterSeq = encounterSeq
                   Encounter = inEncounter
                   Type = consumableClass
                   TypeExplanation = explanation
                   Ability = sp.Spell
                   Target = sp.Base.Target
                   Use = useType })
      | CombatLogEvent.Spell sp when sp.Base.Prefix = SpellPrefix.Spell
                                     && sp.Base.Suffix = SpellSuffix.Energize
                                     && isPlayer sp.Base.Target ->
          match Buffs.recognizeEnergize sp with
          | Some Skip -> ()
          | Some r ->
              result.Add
                ({ EncounterSeq = encounterSeq
                   Encounter = inEncounter
                   Type = r
                   TypeExplanation = "Energize"
                   Ability = sp.Spell
                   Target = sp.Base.Target
                   Use = GainedLost.Used })
          | None -> ()
      | CombatLogEvent.EncounterStart e ->
          inEncounter <- Some e
          encounterSeq <- encounterSeq + 1
      | CombatLogEvent.EncounterEnd _e -> inEncounter <- None
      | _ -> ()

    result |> Seq.toList
