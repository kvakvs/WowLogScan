namespace WowLogScan

open System
open WowLogScan

module Parser =
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

  let parseBuffDebuff (s: string [], index: int): BuffDebuff =
    if s.Length <= index then
      BuffDebuff.Neither
    else
      match s.[index] with
      | "BUFF" -> BuffDebuff.Buff
      | _ -> BuffDebuff.Debuff

  let createSpell (s: string): Ability =
    match s with
    | "0000000000000000" -> Ability.Melee
    | _ -> Ability.Spell s

  let (|Prefix|_|) (p: string) (s: string) =
    if s.StartsWith(p) then Some(s.Substring(p.Length)) else None

  let parseSpellPrefix (s: string): SpellPrefix =
    match s with
    | Prefix "SPELL_PERIODIC_" _ -> SpellPrefix.SpellPeriodic
    | Prefix "SPELL_" _ -> SpellPrefix.Spell
    | Prefix "RANGE_" _ -> SpellPrefix.Range
    | Prefix "SWING_" _ -> SpellPrefix.Swing
    | Prefix "ENVIRONMENTAL_" _ -> SpellPrefix.Environmental
    | Prefix "DAMAGE_SHIELD" _ -> SpellPrefix.DamageShield
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

  let parseTargetedSpell (v: string []): TargetedSpell =
    { Prefix = parseSpellPrefix v.[0]
      Suffix = parseSpellSuffix v.[0]
      Who = createUnit v.[2]
      Target = createUnit v.[6]
      Spell = createSpell v.[10]
      IsBuff = parseBuffDebuff (v, 12) }

  let parseEnchantSpell (v: string []): EnchantSpell =
    { Who = createUnit v.[6]
      Spell = createSpell v.[9]
      ItemName = v.[11] }

  let createDifficulty (d: int): Difficulty =
    match d with
    | 148 -> Difficulty.Classic20
    | 9 -> Difficulty.Classic40
    | _ -> Difficulty.Value d

  let parseEncounter (v: string []): Encounter =
    { Zone = v.[1] |> int
      Boss = createUnit v.[2]
      Difficulty = createDifficulty (v.[3] |> int)
      GroupSize = v.[4] |> int }

  let createOtherEvent (v: string []): CombatLogEvent =
    match v.[0] with
    | "COMBAT_LOG_VERSION" -> CombatLogEvent.CombatLogVersion

    //    | "SPELL_CAST_SUCCESS" -> Event.SpellCastSuccess(parseTargetedSpell v)
//    | "SPELL_CAST_START" -> Event.SpellCastStart(parseTargetedSpell v)
//    | "SPELL_CAST_FAILED" -> Event.SpellCastFailed(parseTargetedSpell v)
//    | "SPELL_HEAL" -> Event.SpellHeal(parseTargetedSpell v)
//    | "SPELL_PERIODIC_HEAL" -> Event.SpellPeriodicHeal(parseTargetedSpell v)
//    | "SPELL_DAMAGE" -> Event.SpellDamage(parseTargetedSpell v)
//    | "SPELL_MISSED" -> Event.SpellMissed(parseTargetedSpell v)
//    | "SPELL_PERIODIC_DAMAGE" -> Event.SpellPeriodicDamage(parseTargetedSpell v)
//    | "SPELL_PERIODIC_MISSED" -> Event.SpellPeriodicMissed(parseTargetedSpell v)
//    | "SPELL_ABSORBED" -> Event.SpellAbsorbed(parseTargetedSpell v)
//    | "SPELL_EXTRA_ATTACKS" -> Event.SpellExtraAttacks(parseTargetedSpell v)
//    | "SPELL_ENERGIZE" -> Event.SpellPeriodicDamage(parseTargetedSpell v)
//    | "SPELL_PERIODIC_ENERGIZE" -> Event.SpellPeriodicDamage(parseTargetedSpell v)
    | "SPELL_DISPEL" ->
        CombatLogEvent.SpellDispel
          { Who = createUnit v.[2]
            Target = createUnit v.[6]
            SpellName = v.[10]
            RemovedSpell = v.[13] }
    //    | "SPELL_SUMMON" -> Event.SpellSummon(parseTargetedSpell v)
//    | "DAMAGE_SHIELD" -> Event.DamageShield(parseTargetedSpell v)
//    | "DAMAGE_SHIELD_MISSED" -> Event.DamageShieldMissed(parseTargetedSpell v)
//    | "SPELL_RESURRECT" -> Event.SpellResurrect(parseTargetedSpell v)

    //    | "RANGE_DAMAGE" -> Event.RangeDamage(parseTargetedSpell v)
//    | "RANGE_MISSED" -> Event.RangeDamage(parseTargetedSpell v)

    //    | "SWING_DAMAGE" -> Event.SwingDamage(parseTargetedSpell v)
//    | "SWING_DAMAGE_LANDED" -> Event.SwingDamageLanded(parseTargetedSpell v)
//    | "SWING_DAMAGE_MISSED" -> Event.SwingDamageMissed(parseTargetedSpell v)
//    | "SWING_MISSED" -> Event.SwingDamageMissed(parseTargetedSpell v)

    | "ENCHANT_APPLIED" -> CombatLogEvent.EnchantApplied(parseEnchantSpell v)
    | "ENCHANT_REMOVED" -> CombatLogEvent.EnchantRemoved(parseEnchantSpell v)
    | "SPELL_DURABILITY_DAMAGE" -> CombatLogEvent.SpellDurabilityDamage(parseEnchantSpell v)

    //    | "ENVIRONMENTAL_DAMAGE" -> Event.EnvironmentalDamage(parseTargetedSpell v)
    | "UNIT_DESTROYED" -> CombatLogEvent.UnitDestroyed(createUnit v.[6])
    | "UNIT_DIED" -> CombatLogEvent.UnitDestroyed(createUnit v.[6])
    | "PARTY_KILL" ->
        CombatLogEvent.PartyKill
          { Victim = createUnit v.[2]
            KilledBy = createUnit v.[6] }
    | "ENCOUNTER_START" -> CombatLogEvent.EncounterStart(parseEncounter v)
    | "ENCOUNTER_END" -> CombatLogEvent.EncounterEnd(parseEncounter v)
    | "COMBATANT_INFO" -> CombatLogEvent.CombatantInfo { PlayerGUID = v.[1]; Equipment = [||] }

    | other -> CombatLogEvent.NotSupported other

  let createEvent (ev: LogLine): CombatLogEvent =
    let v = ev.Values

    match (parseSpellPrefix v.[0]) with
    | SpellPrefix.NotRecognizedPrefix _ -> createOtherEvent v
    | _ -> CombatLogEvent.Spell(parseTargetedSpell v)

  let loadAndParseLogLines (filename: string): LogLine list =
    System.IO.File.ReadAllLines(filename)
    |> Array.toList
    |> List.map parseLine

  let createEventList (logLines: LogLine list): CombatLogEvent list = logLines |> List.map createEvent
