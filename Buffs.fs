namespace WowLogScan


module Buffs =
  open WowLogScan.EventLog

  type WorldBuff =
    | Dragonslayer
    | Rend
    | Zandalar
    | OgreKing
    | Songflower
    | DarkmoonFaire
    | SeasonalHalloween

  type ConsumableClass =
    | WorldBuff // not consumable
    | PotentFlask
    | WeakFlask
    | Potion
    | PotentOffensive
    | WeakOffensive
    | PotentDefensive
    | WeakDefensive
    | Food
    | Unknown // either classify it or Skip
    | Skip

  let recognizeWorldBuff (s: string): Option<WorldBuff> =
    match s with
    | "Rallying Cry of the Dragonslayer" -> Some WorldBuff.Dragonslayer
    | "Warchief's Blessing" -> Some WorldBuff.Rend
    | "Spirit of Zandalar" -> Some WorldBuff.Zandalar
    | "Songflower Serenade" -> Some WorldBuff.Songflower
    | "Fengus' Ferocity" -> Some WorldBuff.OgreKing
    | "Mol'dar's Moxie" -> Some WorldBuff.OgreKing
    | "Slip'kik's Savvy" -> Some WorldBuff.OgreKing
    | "Invocation of the Wickerman" -> Some WorldBuff.SeasonalHalloween
    | s when s.Contains("Sayge's Dark Fortune") -> Some WorldBuff.DarkmoonFaire
    | _ -> None

  // Deprecated TODO: Remove this buff classification
  let recognizeConsumable (a: Ability): Option<ConsumableClass> =
    let spellName =
      match a with
      | Ability.Spell (_id, name) -> name
      | Ability.Spell_ name -> name
      | _ -> ""

    match spellName with
    | "Flask of Titans"
    | "Distilled Wisdom"
    | "Supreme Power" -> Some ConsumableClass.PotentFlask
    | zanza when zanza.EndsWith "of Zanza" -> Some ConsumableClass.WeakFlask
    //-----------------
    | "Ground Scorpok Assay"
    | "Gizzard Gum"
    | "Lung Juice Cocktail"
    | "Cerebral Cortex Compound" -> Some ConsumableClass.WeakFlask
    //-----------------
    | "Elixir of the Mongoose"
    | "Elixir of Superior Defense"
    | "Juju Power"
    | "Juju Might"
    | "Juju Ember"
    | "Juju Chill"
    | "Crystal Ward"
    | "Crystal Spire"
    | "Winterfall Firewater"
    | "Elixir of Giants"
    | "Greater Arcane Elixir"
    | "Elixir of Shadow Power"
    | "Elixir of Greater Firepower"
    | "Elixir of Frost Power"
    | "Mageblood Potion" -> Some ConsumableClass.PotentOffensive
    //-----------------
    | "Major Troll's Blood Potion"
    | "Gift of Arthas"
    | "Elixir of Fortitude" -> Some ConsumableClass.WeakOffensive
    //-----------------
    | pot when pot.EndsWith "Protection Potion" ->
        if pot.StartsWith "Greater" then Some ConsumableClass.PotentDefensive else Some ConsumableClass.WeakDefensive
    //-----------------
    | "Well Fed"
    | "Mana Regeneration"
    | "Increased Stamina"
    | "Increased Intellect"
    | "Blessed Sunfruit Juice"
    | "Blessed Sunfruit"
    | "Strong Alcohol"
    | "Increased Agility" -> Some ConsumableClass.Food
    | _ -> None

  let recognizeEnergize (sp: TargetedSpell): Option<ConsumableClass> =
    let getSpellName s =
      match s with
      | Ability.Spell (_id, name) -> name
      | Ability.Spell_ name -> name
      | _ -> ""

    match sp.Energize with
    | Some energize ->
        let name = getSpellName sp.Spell

        if energize.PowerType = Power.Mana
           && energize.Amount > 890.0
           && name = "Restore Mana" then
          Some ConsumableClass.Potion
        else
          None
    | _ -> None

  let recognizeAura (spellId: SpellId): string * ConsumableClass =
    match spellId with
    | SpellId 24425L -> "Spirit of Zandalar", WorldBuff
    | SpellId 22888L -> "Rallying Cry of the Dragonslayer", WorldBuff
    | SpellId 22817L -> "Fengus' Ferocity", WorldBuff
    | SpellId 22818L -> "Mol'dar's Moxie", WorldBuff
    | SpellId 22820L -> "Slip'kik's Savvy", WorldBuff
    | SpellId 16609L -> "Warchief's Blessing", WorldBuff

    | SpellId 30173L -> "Ground Scorpok Assay", WeakFlask
    | SpellId 30177L -> "Gizzard Gum", WeakFlask
    | SpellId 30164L -> "Lung Juice Cocktail", WeakFlask
    | SpellId 30175L -> "Cerebral Cortex Compound", WeakFlask

    | SpellId 17538L -> "Elixir of the Mongoose", PotentOffensive

    | SpellId 16323L -> "Juju Power", PotentOffensive
    | SpellId 16329L -> "Juju Might", PotentOffensive
    | SpellId 16325L -> "Juju Chill", PotentDefensive
    | SpellId 16326L -> "Juju Ember", PotentDefensive

    | SpellId 11349L -> "Elixir of Greater Def", WeakDefensive
    | SpellId _other -> "", Skip
//    | SpellId other -> (sprintf "Unknown spellid %d" other), Unknown

  let recognizeAbilityAsConsumable (a: Ability): string * ConsumableClass =
    match a with
    | Ability.Spell(id, _text) -> recognizeAura id
    | Ability.Spell_ text -> "", Skip
    | _other -> "", Skip
    