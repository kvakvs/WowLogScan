namespace WowLogScan

module EventLog =
  open WowLogScan.Model.GearPiece
  open CombatlogType
  open System
  open Target

  // Preprocessed log line split into separate string pieces
  type LogLine = { Time: DateTime; Values: string [] }

  type SpellDispel =
    { Who: Unit
      Target: Unit
      SpellName: string
      RemovedSpell: string }

  type EnchantSpell =
    { Who: Unit
      Spell: Ability
      ItemName: string }

  type PartyKill = { Victim: Unit; KilledBy: Unit }

  type Difficulty =
    | Classic20
    | Classic40
    | Value of int64

  type Encounter =
    { Zone: int64
      Boss: Unit
      Difficulty: Difficulty
      GroupSize: int64 }

  type CombatantInfo =
    { Player: Unit
      Equipment: GearPiece list
      Auras: SpellId list }

  type CombatLogEvent =
    | CombatLogVersion

    | Spell of TargetedSpell
    | SpellDispel of SpellDispel

    | EnchantApplied of EnchantSpell
    | EnchantRemoved of EnchantSpell
    | SpellDurabilityDamage of EnchantSpell // force reactive disk

    | UnitDestroyed of Unit
    | UnitDied of Unit
    | PartyKill of PartyKill // player down
    | EncounterStart of Encounter
    | EncounterEnd of Encounter
    | CombatantInfo of CombatantInfo // gear and buffs

    | NotSupported of string
