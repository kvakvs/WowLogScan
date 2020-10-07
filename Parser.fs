namespace WowLogScan

open System
open WowLogScan

module Parser =
  open LogEntity

  let file = @"WoWCombatLog.txt"

  type LogLine = { Time: DateTime; Values: string [] }

  // 9/17 23:45:46.226  COMBATANT_INFO,
  // Player-4678-01668776,357,176,508,74,99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8236,0,(),(0,0,0,0),[],
  // [(16963,76,(2583,0,0),(),()),(19577,65,(),(),()),(16961,76,(2606,0,0),(),()),(4335,37,(),(),()),
  //  (16865,66,(1892,0,0),(),()),(16960,76,(),(),()),(16962,76,(),(),()),(16965,76,(929,0,0),(),()),
  //  (18812,71,(1886,0,0),(),()),(16863,66,(1887,0,0),(),()),(19384,83,(),(),()),(19925,68,(),(),()),
  //  (18406,74,(),(),()),(19431,75,(),(),()),(19907,68,(849,0,0),(),()),(19351,75,(1900,564,0),(),()),
  //  (18168,65,(929,0,0),(),()),(17069,69,(),(),()),(0,0,(),(),())],[]
  let parseCombatantInfoLogLine (line: string): string [] = line.Split([|','|], 26)

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
    let values: string [] = parseLineFields(fields.[0], fields.[3])

    { LogLine.Time = DateTime.Parse(fields.[0] + "/2020 " + fields.[1])
      LogLine.Values = values }

  let REALM_NAME = @"HydraxianWaterlords"

  let createUnit (s: string): Unit =
    if s = "nil" then
      Unit.NoTarget
    else if s.Contains(REALM_NAME) then
      let name = (s.Split '-').[0] // trim "" and take Name from Name-HydraxianWaterlords
      Unit.Player name
    else
      Unit.Npc s

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

  let parseTargetedSpell (v: string []): TargetedSpell =
    { Who = createUnit v.[2]
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

  let createEvent (ev: LogLine): Event =
    let v =
      Array.map (fun (s: string) -> s.Trim('"')) ev.Values

    match v.[0] with
    | "COMBAT_LOG_VERSION" -> Event.CombatLogVersion

    | "SPELL_AURA_APPLIED" -> Event.SpellAuraApplied(parseTargetedSpell v)
    | "SPELL_AURA_REMOVED" -> Event.SpellAuraRemoved(parseTargetedSpell v)
    | "SPELL_AURA_APPLIED_DOSE" -> Event.SpellAuraApplied(parseTargetedSpell v)
    | "SPELL_AURA_REMOVED_DOSE" -> Event.SpellAuraRemoved(parseTargetedSpell v)
    | "SPELL_AURA_REFRESH" -> Event.SpellAuraRefresh(parseTargetedSpell v)
    
    | "SPELL_AURA_BROKEN" -> Event.SpellAuraBroken(parseTargetedSpell v)
    | "SPELL_AURA_BROKEN_SPELL" -> Event.SpellAuraBrokenSpell(parseTargetedSpell v)
    | "SPELL_INTERRUPT" -> Event.SpellInterrupt(parseTargetedSpell v) // kick
    | "SPELL_CREATE" -> Event.SpellCreate(parseTargetedSpell v)
    | "SPELL_INSTAKILL" -> Event.SpellInstakill(parseTargetedSpell v)
    | "SPELL_LEECH" -> Event.SpellLeech(parseTargetedSpell v)
    | "SPELL_PERIODIC_LEECH" -> Event.SpellPeriodicLeech(parseTargetedSpell v)
    | "SPELL_DRAIN" -> Event.SpellDrain(parseTargetedSpell v)
    | "SPELL_PERIODIC_DRAIN" -> Event.SpellPeriodicDrain(parseTargetedSpell v)
    
    | "SPELL_CAST_SUCCESS" -> Event.SpellCastSuccess(parseTargetedSpell v)
    | "SPELL_CAST_START" -> Event.SpellCastStart(parseTargetedSpell v)
    | "SPELL_CAST_FAILED" -> Event.SpellCastFailed(parseTargetedSpell v)
    | "SPELL_HEAL" -> Event.SpellHeal(parseTargetedSpell v)
    | "SPELL_PERIODIC_HEAL" -> Event.SpellPeriodicHeal(parseTargetedSpell v)
    | "SPELL_DAMAGE" -> Event.SpellDamage(parseTargetedSpell v)
    | "SPELL_MISSED" -> Event.SpellMissed(parseTargetedSpell v)
    | "SPELL_PERIODIC_DAMAGE" -> Event.SpellPeriodicDamage(parseTargetedSpell v)
    | "SPELL_PERIODIC_MISSED" -> Event.SpellPeriodicMissed(parseTargetedSpell v)
    | "SPELL_ABSORBED" -> Event.SpellAbsorbed(parseTargetedSpell v)
    | "SPELL_EXTRA_ATTACKS" -> Event.SpellExtraAttacks(parseTargetedSpell v)
    | "SPELL_ENERGIZE" -> Event.SpellPeriodicDamage(parseTargetedSpell v)
    | "SPELL_PERIODIC_ENERGIZE" -> Event.SpellPeriodicDamage(parseTargetedSpell v)
    | "SPELL_DISPEL" ->
        Event.SpellDispel
          { Who = createUnit v.[2]
            Target = createUnit v.[6]
            SpellName = v.[10]
            RemovedSpell = v.[13] }
    | "SPELL_SUMMON" -> Event.SpellSummon(parseTargetedSpell v)
    | "DAMAGE_SHIELD" -> Event.DamageShield(parseTargetedSpell v)
    | "DAMAGE_SHIELD_MISSED" -> Event.DamageShieldMissed(parseTargetedSpell v)
    | "SPELL_RESURRECT" -> Event.SpellResurrect(parseTargetedSpell v)

    | "RANGE_DAMAGE" -> Event.RangeDamage(parseTargetedSpell v)
    | "RANGE_MISSED" -> Event.RangeDamage(parseTargetedSpell v)

    | "SWING_DAMAGE" -> Event.SwingDamage(parseTargetedSpell v)
    | "SWING_DAMAGE_LANDED" -> Event.SwingDamageLanded(parseTargetedSpell v)
    | "SWING_DAMAGE_MISSED" -> Event.SwingDamageMissed(parseTargetedSpell v)
    | "SWING_MISSED" -> Event.SwingDamageMissed(parseTargetedSpell v)

    | "ENCHANT_APPLIED" -> Event.EnchantApplied(parseEnchantSpell v)
    | "ENCHANT_REMOVED" -> Event.EnchantRemoved(parseEnchantSpell v)
    | "SPELL_DURABILITY_DAMAGE" -> Event.SpellDurabilityDamage(parseEnchantSpell v)

    | "ENVIRONMENTAL_DAMAGE" -> Event.EnvironmentalDamage(parseTargetedSpell v)
    | "UNIT_DESTROYED" -> Event.UnitDestroyed(createUnit v.[6])
    | "UNIT_DIED" -> Event.UnitDestroyed(createUnit v.[6])
    | "PARTY_KILL" ->
        Event.PartyKill
          { Victim = createUnit v.[2]
            KilledBy = createUnit v.[6] }
    | "ENCOUNTER_START" -> Event.EncounterStart(parseEncounter v)
    | "ENCOUNTER_END" -> Event.EncounterEnd(parseEncounter v)
    | "COMBATANT_INFO" -> Event.CombatantInfo { PlayerGUID = v.[1]; Equipment = [||] }

    | other -> Event.NotSupported other

  let readLogLines (indexFrom: int, indexTo: int): Event list =
    System.IO.File.ReadAllLines(file).[indexFrom..indexTo]
    |> Array.toList
    |> List.map parseLine
    |> List.map createEvent

//  let countFailed allLines =
//    let rec readLines (myLines: string list) resultItem resultdata =
//      match myLines with
//      | h :: t when h.Contains("==") ->
//          let myItem = h.Substring(3, h.Length - 6)
//          readLines t (myItem :: resultItem) resultdata
//      | total :: state :: t when total.Contains("Total:")
//                                 && state.StartsWith("Failed") ->
//          let info =
//            (List.head resultItem), total.Substring(7) //, state ...etc information
//
//          readLines t resultItem (info :: resultdata)
//      | h :: t -> readLines t resultItem resultdata
//      | [] -> resultdata
//
//    readLines (allLines) [] []
