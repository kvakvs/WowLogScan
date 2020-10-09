namespace WowLogScan.Model

open WowLogScan.CombatlogType

module GearPiece =

  type EquipmentSlot =
    | Head | Neck | Shoulder | Chest | Waist | Legs | Feet | Wrist
    | Hands | Ring1 | Ring2 | Trinket1 | Trinket2 | Shirt
    | Cloak | MainHand | OffHand | Ranged | Unknown of int
    
  
  type GearPiece =
    { ItemId: int64
      ItemLevel: int64
      Enchants: CLToken list
      SlotId: EquipmentSlot }

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
    | _ -> Unknown s