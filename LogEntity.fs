namespace WowLogScan

module LogEntity =
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

  type TargetedSpell =
    { Who: Unit
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
    
    | SpellAuraApplied of TargetedSpell
    | SpellAuraRemoved of TargetedSpell
    | SpellAuraAppliedDose of TargetedSpell
    | SpellAuraRemovedDose of TargetedSpell
    | SpellAuraRefresh of TargetedSpell
    | SpellAuraBroken of TargetedSpell // fear
    | SpellAuraBrokenSpell of TargetedSpell // interrupt with ability
    | SpellInterrupt of TargetedSpell // kick/pummel 
    | SpellCreate of TargetedSpell // spawn something (Kurinnaxx sand trap?)
    | SpellInstakill of TargetedSpell // demonic sacrifice
    | SpellLeech of TargetedSpell // obsidian destroyer from player
    | SpellPeriodicLeech of TargetedSpell  
    | SpellDrain of TargetedSpell // one time mana drain
    | SpellPeriodicDrain of TargetedSpell  

    | SpellCastSuccess of TargetedSpell
    | SpellCastStart of TargetedSpell
    | SpellCastFailed of TargetedSpell
    | SpellHeal of TargetedSpell
    | SpellDamage of TargetedSpell
    | SpellMissed of TargetedSpell
    | SpellDispel of SpellDispel
    | SpellEnergize of TargetedSpell
    | SpellPeriodicHeal of TargetedSpell
    | SpellPeriodicDamage of TargetedSpell
    | SpellPeriodicEnergize of TargetedSpell
    | SpellPeriodicMissed of TargetedSpell
    | SpellAbsorbed of TargetedSpell
    | SpellSummon of TargetedSpell
    | SpellExtraAttacks of TargetedSpell
    | SpellResurrect of TargetedSpell

    | EnchantApplied of EnchantSpell
    | EnchantRemoved of EnchantSpell
    | SpellDurabilityDamage of EnchantSpell // force reactive disk

    | DamageShield of TargetedSpell // thorns
    | DamageShieldMissed of TargetedSpell // thorns resist

    | UnitDestroyed of Unit
    | UnitDied of Unit
    | PartyKill of PartyKill // player down
    | EncounterStart of Encounter
    | EncounterEnd of Encounter
    | CombatantInfo of CombatantInfo // gear and buffs

    | RangeDamage of TargetedSpell
    | RangeMissed of TargetedSpell
    | EnvironmentalDamage of TargetedSpell

    | SwingDamage of TargetedSpell
    | SwingDamageLanded of TargetedSpell
    | SwingDamageMissed of TargetedSpell
    | SwingMissed of TargetedSpell

    | NotSupported of string
