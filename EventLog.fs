namespace WowLogScan

module EventLog =
  type Unit =
    | Player of string
    | Npc of string
    | NoTarget

  type BuffDebuff =
    | Buff
    | Debuff
    | Neither

  type Ability =
    | Spell of string
    | Melee

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
    | Damage | DamageLanded | Missed | DamageShield
    | Heal | HealAbsorbed
    | Energize | Drain | Leech
    | Dispel | DispelFailed
    | ExtraAttacks
    | AuraApplied | AuraRemoved | AuraAppliedDose | AuraRemovedDose | AuraRefresh
    | Interrupt | AuraBroken | AuraBrokenSpell // fear and interrupt
    | CastStart | CastSuccess | CastFailed
    | Instakill
    | DurabilityDamage | DurabilityDamageAll
    | Create // spawn under player
    | Summon
    | Resurrect
    | Absorbed
    | NotRecognizedSuffix of string
  
  type TargetedSpell =
    { Prefix: SpellPrefix
      Suffix: SpellSuffix
      Who: Unit
      Target: Unit
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
    | Value of int
  
  type Encounter =
    { Zone: int
      Boss: Unit
      Difficulty: Difficulty
      GroupSize: int }
    
  type EquipmentItem = {
    ItemId: int
    ItemLevel: int
  }
    
  type CombatantInfo = {
    PlayerGUID: string
    Equipment: EquipmentItem[]
  }

  type Event =
    | CombatLogVersion
    
    | Spell of TargetedSpell
    | SpellDispel of SpellDispel

    | EnchantApplied of EnchantSpell
    | EnchantRemoved of EnchantSpell
    | SpellDurabilityDamage of EnchantSpell // force reactive disk

//    | DamageShield of TargetedSpell // thorns
//    | DamageShieldMissed of TargetedSpell // thorns resist

    | UnitDestroyed of Unit
    | UnitDied of Unit
    | PartyKill of PartyKill // player down
    | EncounterStart of Encounter
    | EncounterEnd of Encounter
    | CombatantInfo of CombatantInfo // gear and buffs

//    | RangeDamage of TargetedSpell
//    | RangeMissed of TargetedSpell
//    | EnvironmentalDamage of TargetedSpell

//    | SwingDamage of TargetedSpell
//    | SwingDamageLanded of TargetedSpell
//    | SwingDamageMissed of TargetedSpell
//    | SwingMissed of TargetedSpell

    | NotSupported of string
