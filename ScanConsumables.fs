namespace WowLogScan

open WowLogScan.Model.Unit

module ScanConsumables =
  open EventLog
  open RaidState

  let isPlayer (u: Unit): bool =
    match u with
    | Player _ -> true
    | PlayerId _ -> true
    | _ -> false

  let formatEncounter (e: Option<Encounter>): string =
    match e with
    | Some encounter -> sprintf "%A" encounter.Boss
    | None -> "<trash>"

  let scanConsumables (_raid: RaidState, events: CombatLogEvent list) =
    let mutable encounter = 0
    let mutable inEncounter: Option<Encounter> = None

    for ev in events do
      match ev with
      | CombatLogEvent.Spell sp when sp.Base.Prefix = SpellPrefix.Spell
                                     && sp.Base.Suffix = SpellSuffix.AuraApplied
                                     && isPlayer sp.Base.Target ->
          match Buffs.recognizeConsumable sp.Spell with
          | Some r ->
              printfn
                "%s encounter %d: %A %A %A"
                (formatEncounter inEncounter)
                encounter
                sp.Base.Target
                sp.Spell
                r
          | None -> ()
      | CombatLogEvent.Spell sp when sp.Base.Prefix = SpellPrefix.Spell
                                     && sp.Base.Suffix = SpellSuffix.Energize
                                     && isPlayer sp.Base.Target ->
          match Buffs.recognizeEnergize sp with
          | Some r ->
              printfn
                "%s encounter %d: %A %A %A"
                (formatEncounter inEncounter)
                encounter
                sp.Base.Target
                sp.Spell
                r
          | None -> ()
      | CombatLogEvent.EncounterStart e ->
          inEncounter <- Some e
          encounter <- encounter + 1
      | CombatLogEvent.EncounterEnd _e -> inEncounter <- None
      | _ -> ()
