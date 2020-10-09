namespace WowLogScan

module Parser =
  open WowLogScan.Model.GearPiece
  open System
  open WowLogScan
  open WowLogScan.CombatlogType
  open EventLog
  open Model.Unit

  // 9/17 23:45:46.226  COMBATANT_INFO,
  // Player-4678-01668776,357,176,508,74,99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8236,0,(),(0,0,0,0),[],
  // [(16963,76,(2583,0,0),(),()),(19577,65,(),(),()),(16961,76,(2606,0,0),(),()),(4335,37,(),(),()),
  //  (16865,66,(1892,0,0),(),()),(16960,76,(),(),()),(16962,76,(),(),()),(16965,76,(929,0,0),(),()),
  //  (18812,71,(1886,0,0),(),()),(16863,66,(1887,0,0),(),()),(19384,83,(),(),()),(19925,68,(),(),()),
  //  (18406,74,(),(),()),(19431,75,(),(),()),(19907,68,(849,0,0),(),()),(19351,75,(1900,564,0),(),()),
  //  (18168,65,(929,0,0),(),()),(17069,69,(),(),()),(0,0,(),(),())],[]
  let parseCombatantInfoLogLine (line: string): string [] = line.Split([| ',' |], 26)

  // Parse data section of the event line, which looks like CSV
  // Most events need just comma split, but COMBATANT_INFO is special
  let parseLineFields (event: string, line: string): string [] =
    match event with
    | "COMBATANT_INFO" -> parseCombatantInfoLogLine line
    | _ -> line.Split ','

  // Given a string of Date/Time/CSV fields parse
  let parseLine (line: string): LogLine =
    let fields: string [] = line.Split([| ' ' |], 4)

    // there is double space after the time, so fields[2] is "" skip it
    let values: string [] = parseLineFields (fields.[0], fields.[3])

    // Trim "quotation" marks from all strings
    let trimmedValue =
      Array.map (fun (s: string) -> s.Trim('"')) values

    { LogLine.Time = DateTime.Parse(fields.[0] + "/2020 " + fields.[1])
      LogLine.Values = trimmedValue }

  let parseBuffDebuff (s: CLToken [], index: int): BuffDebuff =
    if s.Length <= index then
      BuffDebuff.Neither
    else
      match s.[index] with
      | CLToken.String "BUFF" -> BuffDebuff.Buff
      | _ -> BuffDebuff.Debuff

  let createSpell (s: string): Ability =
    match s with
    | "0000000000000000" -> Ability.Melee
    | _ -> Ability.Spell s

  let (|Prefix|_|) (p: string) (s: string) =
    if s.StartsWith(p) then Some(s.Substring(p.Length)) else None

  let parseSpellPrefix (s: string): SpellPrefix =
    match s with
    | Prefix "DAMAGE_SHIELD" _ -> SpellPrefix.DamageShield
    | Prefix "SPELL_PERIODIC_" _ -> SpellPrefix.SpellPeriodic
    | Prefix "SPELL_" _ -> SpellPrefix.Spell
    | Prefix "RANGE_" _ -> SpellPrefix.Range
    | Prefix "SWING_" _ -> SpellPrefix.Swing
    | Prefix "ENVIRONMENTAL_" _ -> SpellPrefix.Environmental
    | _ -> SpellPrefix.NotRecognizedPrefix s

  let parseSpellSuffix (s: string): SpellSuffix =
    match s with
    | s when s.EndsWith("_DAMAGE") -> SpellSuffix.Damage
    | s when s.EndsWith("DAMAGE_SHIELD") -> SpellSuffix.DamageShield
    | s when s.EndsWith("_DAMAGE_LANDED") -> SpellSuffix.DamageLanded
    | s when s.EndsWith("_HEAL") -> SpellSuffix.Heal
    | s when s.EndsWith("_HEAL_ABSORBED") -> SpellSuffix.HealAbsorbed
    | s when s.EndsWith("_LEECH") -> SpellSuffix.Leech
    | s when s.EndsWith("_DRAIN") -> SpellSuffix.Drain
    | s when s.EndsWith("_ENERGIZE") -> SpellSuffix.Energize
    | s when s.EndsWith("_MISSED") -> SpellSuffix.Missed
    | s when s.EndsWith("_ABSORBED") -> SpellSuffix.Absorbed
    | s when s.EndsWith("_EXTRA_ATTACKS") -> SpellSuffix.ExtraAttacks
    | s when s.EndsWith("_AURA_APPLIED") -> SpellSuffix.AuraApplied
    | s when s.EndsWith("_AURA_REMOVED") -> SpellSuffix.AuraRemoved
    | s when s.EndsWith("_AURA_APPLIED_DOSE") -> SpellSuffix.AuraAppliedDose
    | s when s.EndsWith("_AURA_REMOVED_DOSE") -> SpellSuffix.AuraRemovedDose
    | s when s.EndsWith("_AURA_REFRESH") -> SpellSuffix.AuraRefresh
    | s when s.EndsWith("_AURA_BROKEN") -> SpellSuffix.AuraBroken
    | s when s.EndsWith("_AURA_BROKEN_SPELL") -> SpellSuffix.AuraBrokenSpell
    | s when s.EndsWith("_INTERRUPT") -> SpellSuffix.Interrupt
    | s when s.EndsWith("_CREATE") -> SpellSuffix.Create
    | s when s.EndsWith("_INSTAKILL") -> SpellSuffix.Instakill
    | s when s.EndsWith("_CAST_START") -> SpellSuffix.CastStart
    | s when s.EndsWith("_CAST_SUCCESS") -> SpellSuffix.CastSuccess
    | s when s.EndsWith("_CAST_FAILED") -> SpellSuffix.CastFailed
    | s when s.EndsWith("_SUMMON") -> SpellSuffix.Summon
    | s when s.EndsWith("_RESURRECT") -> SpellSuffix.Resurrect
    | _ -> SpellSuffix.NotRecognizedSuffix s

  let createEnvironmentalDamageSource (s: CLToken): Ability =
    match s with
    | CLToken.Environmental dt -> Ability.Environmental dt
    | _ -> failwithf "Expected environmental damage type, while got %A" s

  let parseAbility (v: CLToken []): Ability =
    let eventName = extractString v.[0]
    let prefix = parseSpellPrefix eventName
    let suffix = parseSpellSuffix eventName

    match prefix, suffix with
    | (SpellPrefix.Spell, SpellSuffix.Absorbed) ->
        match v.[14] with
        | CLToken.String s -> createSpell s
        | _ -> extractString v.[17] |> createSpell
    | (SpellPrefix.Spell, _) -> extractString v.[10] |> createSpell
    | (SpellPrefix.SpellPeriodic, _) -> extractString v.[10] |> createSpell
    | (SpellPrefix.Swing, _) -> Ability.Melee
    | (SpellPrefix.Range, _) -> extractString v.[10] |> createSpell
    | (SpellPrefix.Environmental, _) -> createEnvironmentalDamageSource v.[25]
    | (SpellPrefix.DamageShield, _) -> extractString v.[10] |> createSpell
    | _ -> failwithf "Ability prefix is not recognized. Got %A" v

  let parseBaseParams (v: CLToken []): SpellBaseParams =
    let eventName = extractString v.[0]
    let prefix = parseSpellPrefix eventName
    let suffix = parseSpellSuffix eventName

    { Prefix = prefix
      Suffix = suffix
      Who = unitFromToken v.[2]
      Target = unitFromToken v.[6] }

  let parseSpell (v: CLToken []): TargetedSpell =
    { Base = parseBaseParams v
      Spell = parseAbility v
      IsBuff = parseBuffDebuff (v, 12) }

  let parseEnchantSpell (v: CLToken []): EnchantSpell =
    { Who = unitFromToken v.[6]
      Spell = extractString v.[9] |> createSpell
      ItemName = extractString v.[11] }

  let createDifficulty (d: int64): Difficulty =
    match d with
    | 148L -> Difficulty.Classic20
    | 9L -> Difficulty.Classic40
    | _ -> Difficulty.Value d

  let parseEncounter (v: CLToken []): Encounter =
    { Zone = extractInt v.[1]
      Boss = unitFromToken v.[2]
      Difficulty = createDifficulty (extractInt v.[3])
      GroupSize = extractInt v.[4] }

  let parseGearPiece (t: CLToken, slotId: int): GearPiece =
    match t with
    | CLToken.List itemParams ->
      {SlotId = createEquipmentSlot slotId
       ItemId = extractInt itemParams.[0]
       ItemLevel = extractInt itemParams.[1]
       Enchants = extractList itemParams.[2] }
    | _  -> failwithf "Error while parsing gear piece of a combatant, required list, got %A" t

  let parseCombatantGear (g: CLToken list): GearPiece list =
    List.mapi (fun i value -> parseGearPiece(value, i)) g

  let parseCombatantInfo (v: CLToken []): CombatantInfo =
    let gear = extractList v.[28] |> parseCombatantGear
    { Player = unitFromToken v.[1]
      Equipment = gear }

  let createOtherEvent (v: CLToken []): CombatLogEvent =
    match extractString v.[0] with
    | "COMBAT_LOG_VERSION" -> CombatLogEvent.CombatLogVersion
    | "SPELL_DISPEL" ->
        CombatLogEvent.SpellDispel
          { Who = unitFromToken v.[2]
            Target = unitFromToken v.[6]
            SpellName = extractString v.[10]
            RemovedSpell = extractString v.[13] }

    | "ENCHANT_APPLIED" -> CombatLogEvent.EnchantApplied(parseEnchantSpell v)
    | "ENCHANT_REMOVED" -> CombatLogEvent.EnchantRemoved(parseEnchantSpell v)
    | "SPELL_DURABILITY_DAMAGE" -> CombatLogEvent.SpellDurabilityDamage(parseEnchantSpell v)

    | "UNIT_DESTROYED" -> CombatLogEvent.UnitDestroyed(unitFromToken v.[6])
    | "UNIT_DIED" -> CombatLogEvent.UnitDestroyed(unitFromToken v.[6])
    | "PARTY_KILL" ->
        CombatLogEvent.PartyKill
          { Victim = unitFromToken v.[2]
            KilledBy = unitFromToken v.[6] }
    | "ENCOUNTER_START" -> CombatLogEvent.EncounterStart(parseEncounter v)
    | "ENCOUNTER_END" -> CombatLogEvent.EncounterEnd(parseEncounter v)
    | "COMBATANT_INFO" ->
      let ci = parseCombatantInfo v
      CombatLogEvent.CombatantInfo(ci)

    | other -> CombatLogEvent.NotSupported other

  let createEvent (ev: CLEvent, line: int): CombatLogEvent =
    let args = ev.Args

    try
      match args.[0] with
      | CLToken.String pfx ->
          match parseSpellPrefix pfx with
          | SpellPrefix.NotRecognizedPrefix _ -> createOtherEvent args
          | _ -> CombatLogEvent.Spell(parseSpell args)
      | _Other -> failwithf "First token in an event after date/time must be a string, instead got %A" ev
    with err -> failwithf "Err (line %d) %A" line err
