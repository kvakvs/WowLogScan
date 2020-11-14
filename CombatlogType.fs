namespace WowLogScan

module CombatlogType =
  open TargetType
  
  type Unit = TargetType.Unit
  
  type SpellId = SpellId of int64

  type EnvDamageType = Falling

  type Ability =
    | Spell of SpellId * string
    | Spell_ of string // spell with text only, is this useful?
    | Melee
    | Environmental of EnvDamageType

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
      Caster: Unit
      Target: Unit }

  type Power =
    | Mana
    | Rage
    | Focus
    | Energy
    | Combo
    | Other

  type Energize =
    { Amount: float
      OverEnergize: float
      PowerType: Power }

  type BuffDebuff =
    | Buff
    | Debuff
    | Neither

  type TargetedSpell =
    { Base: SpellBaseParams
      Spell: Ability
      IsBuff: BuffDebuff
      Energize: Option<Energize> }
