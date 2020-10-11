namespace WowLogScan.Model

module GearPiece =

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
    | Enchant of int64 * string
    | Unknown of int64
    | None

  let lookupEnchantmentId (id: int64): Option<string> =
    match id with
    | 33L -> Some "+3 Scope"
    | 34L -> Some "+2% Haste Counterweight"
    | 247L -> Some "+1 Agility"
    | 464L -> Some "+4% Mount"
    | 564L -> Some "Windfury Totem 3"
    | 625L -> Some "Instant Poison 6"
    | 643L -> Some "Mind Numbing Poison 3"
    | 664L -> Some "+7 Scope"
    | 804L -> Some "+10 Shadow Res"
    | 849L -> Some "+3 Agility"
    | 851L -> Some "+5 Spirit"
    | 884L -> Some "+50 Armor"
    | 903L -> Some "+3 All Res"
    | 904L -> Some "+5 Agility"
    | 905L -> Some "+5 Intellect"
    | 906L -> Some "+5 Mining"
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
    | 2583L -> Some "+10 Def +10 Stamina +30 Block"
    | 2585L -> Some "+28 AttackP +1% Dodge"
    | 2586L -> Some "+24 Ranged +10 Stamina +1% Hit"
    | 2587L -> Some "+13 Spell +15 Intellect"
    | 2588L -> Some "+18 Spell +1% Hit"
    | 2589L -> Some "+18 Spell +10 Stamina"
    | 2590L -> Some "+13 Spell +10 Stamina +5 Mana/5"
    | 2604L
    | 2605L -> Some "+18 Spell"
    | 2606L -> Some "+30 AttackP"
    | 2613L -> Some "+2% Threat"
    | 2614L -> Some "+20 Shadow"
    | 2619L -> Some "+15 Fire Res"
    | 2617L -> Some "+16 Spell"
    | 2620L -> Some "+15 Nature Res"
    | 2623L -> Some "Minor Wizard Oil"
    | 2625L -> Some "Lesser Mana Oil"
    | 2627L -> Some "Wizard Oil"
    | 2628L -> Some "Brilliant Wizard Oil"
    | 2629L -> Some "Brilliant Mana Oil"
    | 2646L -> Some "+25 Agility"
    | _ ->
        printfn "Unknown enchantmentId %A please update" id
        Option.None

  let createEnchantment (id: int64): Enchantment =
    if id = 0L then
      Enchantment.None
    else
      match lookupEnchantmentId id with
      | Some name -> Enchantment.Enchant(id, name)
      | Option.None -> Enchantment.Unknown id

  type GearPiece =
    { ItemId: int64
      ItemLevel: int64
      Enchants: Enchantment list
      SlotId: EquipmentSlot }
