﻿namespace WowLogScan.Model


module GearPiece =
  open WowLogScan.Buffs

  type EquipmentSlot =
    | Head
    | Neck
    | Shoulder
    | Chest
    | Waist
    | Legs
    | Feet
    | Wrist
    | Hands
    | Ring1
    | Ring2
    | Trinket1
    | Trinket2
    | Shirt
    | Cloak
    | MainHand
    | OffHand
    | Ranged
    | Tabard
    | Unknown of int

  let createEquipmentSlot (s: int): EquipmentSlot =
    match s with
    | 0 -> Head
    | 1 -> Neck
    | 2 -> Shoulder
    | 3 -> Shirt
    | 4 -> Chest
    | 5 -> Waist
    | 6 -> Legs
    | 7 -> Feet
    | 8 -> Wrist
    | 9 -> Hands
    | 10 -> Ring1
    | 11 -> Ring2
    | 12 -> Trinket1
    | 13 -> Trinket2
    | 14 -> Cloak
    | 15 -> MainHand
    | 16 -> OffHand
    | 17 -> Ranged
    | 18 -> Tabard
    | _ -> Unknown s

  let isSlotEnchantable (s: EquipmentSlot): bool =
    match s with
    | EquipmentSlot.Head
    | EquipmentSlot.Shoulder
    | EquipmentSlot.Chest
    | EquipmentSlot.Legs
    | EquipmentSlot.Feet
    | EquipmentSlot.Wrist
    | EquipmentSlot.Hands
    | EquipmentSlot.Cloak
    | EquipmentSlot.MainHand
    | EquipmentSlot.OffHand
    | EquipmentSlot.Ranged -> true
    | _ -> false

  type Enchantment =
    | Enchant of int64
    | None

  let lookupEnchantmentId (id: int64): Option<string> =
    match id with
    | 15L -> Some "+8 Armor Kit"
    | 16L -> Some "+16 Armor Kit"
    | 18L -> Some "+32 Armor Kit"
    | 33L -> Some "+3 Scope"
    | 34L -> Some "+2% Haste Counterweight"
    | 66L -> Some "+1 Stamina"
    | 247L -> Some "+1 Agility"
    | 464L -> Some "+4% Mount"
    | 564L -> Some "Windfury Totem 3"
    | 625L -> Some "Instant Poison 6"
    | 643L -> Some "Mind Numbing Poison 3"
    | 664L -> Some "+7 Scope"
    | 804L -> Some "+10 Shadow Res"
    | 849L -> Some "+3 Agility"
    | 850L -> Some "+35 Health"
    | 851L -> Some "+5 Spirit"
    | 856L -> Some "+5 Strength"
    | 884L -> Some "+50 Armor"
    | 903L -> Some "+3 All Res"
    | 904L -> Some "+5 Agility"
    | 905L -> Some "+5 Intellect"
    | 906L -> Some "+5 Mining"
    | 907L -> Some "+7 Spirit"
    | 909L -> Some "+5 Herb"
    | 911L -> Some "+8% Run"
    | 927L -> Some "+7 Strength"
    | 928L -> Some "+3 Stats"
    | 929L -> Some "+7 Stamina"
    | 930L -> Some "+2% Mount"
    | 931L -> Some "+1% Haste"
    | 963L -> Some "+7 Weapon"
    | 1503L -> Some "+100 Health"
    | 1505L -> Some "+20 Fire Res"
    | 1506L -> Some "+8 Strength"
    | 1508L -> Some "+8 Agility"
    | 1643L -> Some "Sharpened +8"
    | 1664L -> Some "Rockbiter 7"
    | 1666L -> Some "Flametongue 6"
    | 1669L -> Some "Windfury 4"
    | 1704L -> Some "Thorium Spike"
    | 1843L -> Some "+40 Armor Kit"
    | 1883L -> Some "+7 Intellect"
    | 1884L -> Some "+9 Spirit"
    | 1885L -> Some "+9 Strength"
    | 1886L -> Some "+9 Stamina"
    | 1887L -> Some "+7 Agility"
    | 1888L -> Some "+5 All Res"
    | 1889L -> Some "+70 Armor"
    | 1891L -> Some "+4 Stats"
    | 1892L -> Some "+100 Health"
    | 1893L -> Some "+100 Mana"
    | 1894L -> Some "Icy Weapon"
    | 1900L -> Some "Crusader"
    | 1903L -> Some "+9 Spirit"
    | 2463L -> Some "+7 Fire Res"
    | 2483L -> Some "+5 Fire Res"
    | 2486L -> Some "+5 Nature Res"
    | 2487L -> Some "+5 Shadow Res"
    | 2504L -> Some "+30 Spell"
    | 2505L -> Some "+29 Spell"
    | 2506L -> Some "Sharpened +2% Crit"
    | 2523L -> Some "+3% Ranged Hit"
    | 2543L -> Some "+1% Haste"
    | 2544L -> Some "+8 Spell"
    | 2545L -> Some "+1% Dodge"
    | 2564L -> Some "+15 Agility"
    | 2565L -> Some "+5 Mana/5"
    | 2566L -> Some "+13 Spell"
    | 2567L -> Some "+20 Spirit"
    | 2568L -> Some "+22 Intellect"
    | 2583L -> Some "+10 Def +10 Stamina +30 Block"
    | 2585L -> Some "+28 AttackP +1% Dodge"
    | 2586L -> Some "+24 Ranged +10 Stamina +1% Hit"
    | 2587L -> Some "+13 Spell +15 Intellect"
    | 2588L -> Some "+18 Spell +1% Hit"
    | 2589L -> Some "+18 Spell +10 Stamina"
    | 2590L -> Some "+13 Spell +10 Stamina +5 Mana/5"
    | 2591L -> Some "+10 Intellect +10 Stamina +12 Spell"
    | 2604L
    | 2605L -> Some "+18 Spell"
    | 2606L -> Some "+30 AttackP"
    | 2613L -> Some "+2% Threat"
    | 2614L -> Some "+20 Shadow"
    | 2616L -> Some "+20 Fire"
    | 2619L -> Some "+15 Fire Res"
    | 2617L -> Some "+16 Spell"
    | 2620L -> Some "+15 Nature Res"
    | 2621L -> Some "+2% Threat Reduce"
    | 2622L -> Some "+1% Dodge"
    | 2623L -> Some "Minor Wizard Oil"
    | 2625L -> Some "Lesser Mana Oil"
    | 2626L -> Some "Lesser Wizard Oil"
    | 2627L -> Some "Wizard Oil"
    | 2628L -> Some "Brilliant Wizard Oil"
    | 2629L -> Some "Brilliant Mana Oil"
    | 2646L -> Some "+25 Agility"
    | _ ->
        printfn "Unknown enchantmentId %A please update" id
        Option.None

  let explainEnchantment (e: Enchantment): string =
    match e with
    | Enchant id ->
        match lookupEnchantmentId id with
        | Some s -> s
        | Option.None -> "?"
    | _ -> "?"

  let recognizeEnchantment (e: Enchantment): ConsumableClass * string =
    match e with
    | Enchant 2623L
    | Enchant 2625L
    | Enchant 2626L
    | Enchant 2627L -> (ConsumableClass.WeakOffensive, explainEnchantment e)
    | Enchant 2628L
    | Enchant 2629L -> (ConsumableClass.PotentOffensive, explainEnchantment e)
    | _ -> ConsumableClass.Skip, ""

  let createEnchantment (id: int64): Enchantment =
    if id = 0L then Enchantment.None else Enchantment.Enchant id

  type GearPiece =
    { ItemId: int64
      ItemLevel: int64
      Enchants: Enchantment list
      SlotId: EquipmentSlot }
