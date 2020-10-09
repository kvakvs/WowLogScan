namespace WowLogScan


module EventLog =
  open WowLogScan.CombatlogType
  open System
  open Model.Unit

  // Preprocessed log line split into separate string pieces
  type LogLine = { Time: DateTime; Values: string [] }

  type BuffDebuff =
    | Buff
    | Debuff
    | Neither

  type Ability =
    | Spell of string
    | Melee
    | Environmental of EnvDamageType

  type SpellDispel =
    { Who: Unit
      Target: Unit
      SpellName: string
      RemovedSpell: string }

  type SpellPrefix =
    | Swing
    | Range
    | Spell
    | DamageShield
    | SpellPeriodic
    | Environmental
    | NotRecognizedPrefix of string

  type SpellSuffix =
    | Damage
    | DamageLanded
    | Missed
    | DamageShield
    | Heal
    | HealAbsorbed
    | Energize
    | Drain
    | Leech
    | Dispel
    | DispelFailed
    | ExtraAttacks
    | AuraApplied
    | AuraRemoved
    | AuraAppliedDose
    | AuraRemovedDose
    | AuraRefresh
    | Interrupt
    | AuraBroken
    | AuraBrokenSpell // fear and interrupt
    | CastStart
    | CastSuccess
    | CastFailed
    | Instakill
    | DurabilityDamage
    | DurabilityDamageAll
    | Create // spawn under player
    | Summon
    | Resurrect
    | Absorbed
    | NotRecognizedSuffix of string


  type SpellBaseParams =
    { Prefix: SpellPrefix
      Suffix: SpellSuffix
      Who: Unit
      Target: Unit }

  type TargetedSpell =
    { Base: SpellBaseParams
      Spell: Ability
      IsBuff: BuffDebuff }

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

  type EquipmentItem = { ItemId: int64; ItemLevel: int64 }

  type CombatantInfo =
    { Player: Unit
      Equipment: EquipmentItem [] }

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
