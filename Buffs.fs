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

  type ConsumableClass =
    | PotentFlask
    | WeakFlask
    | Potion
    | PotentElixir
    | WeakElixir
    | PotentDefensive
    | WeakDefensive
    | Food

  let recognizeBuff (s: string): Option<WorldBuff> =
    match s with
    | "Rallying Cry of the Dragonslayer" -> Some WorldBuff.Dragonslayer
    | "Warchief's Blessing" -> Some WorldBuff.Rend
    | "Spirit of Zandalar" -> Some WorldBuff.Zandalar
    | "Songflower Serenade" -> Some WorldBuff.Songflower
    | "Fengus' Ferocity" -> Some WorldBuff.OgreKing
    | "Mol'dar's Moxie" -> Some WorldBuff.OgreKing
    | "Slip'kik's Savvy" -> Some WorldBuff.OgreKing
    | s when s.Contains("Sayge's Dark Fortune") -> Some WorldBuff.DarkmoonFaire
    | _ -> None

  let recognizeConsumable (a: Ability): Option<ConsumableClass> =
    match a with
    | Ability.Spell name ->
        match name with
        | flask when flask.StartsWith "Flask of" -> Some ConsumableClass.PotentFlask
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
        | "Mageblood Potion" -> Some ConsumableClass.PotentElixir
        //-----------------
        | "Major Troll's Blood Potion"
        | "Gift of Arthas"
        | "Elixir of Fortitude" -> Some ConsumableClass.WeakElixir
        //-----------------
        | pot when pot.EndsWith "Protection Potion" ->
            if pot.StartsWith "Greater" then
              Some ConsumableClass.PotentDefensive
            else
              Some ConsumableClass.WeakDefensive
        //-----------------
        | "Well Fed"
        | "Mana Regeneration"
        | "Increased Stamina"
        | "Increased Intellect"
        | "Blessed Sunfruit Juice"
        | "Blessed Sunfruit"
        | "Strong Alcohol" // DM alcohol, rumsey rum
        | "Increased Agility" -> Some ConsumableClass.Food
        | _ -> None
    | _ -> None

  let recognizeEnergize (sp: TargetedSpell): Option<ConsumableClass> =
    let getSpellName s =
      match s with
      | Ability.Spell name -> name
      | _ -> ""
      
    match sp.Energize with
    | Some energize ->
      let name = getSpellName sp.Spell
      if energize.PowerType = Power.Mana && energize.Amount > 890.0 && name = "Restore Mana" then
        Some ConsumableClass.Potion
      else
        None
    | _ -> None
