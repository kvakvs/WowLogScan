namespace WowLogScan

module ScanEnchants =
  open System
  open System.Collections.Generic
  open WowLogScan.Model.GearPiece
  open Target
  open Microsoft.FSharp.Collections
  open EventLog
  open RaidState

  type PlayerItemsPerSlot = Dictionary<int64, GearPiece>
  type AllEquipmentBySlot = Dictionary<EquipmentSlot, PlayerItemsPerSlot>

  type EnchantReportItem =
    { Player: TargetType.Unit
      Gear: AllEquipmentBySlot }

  let printReport (e: EnchantReportItem): string =
    let formatItem (g: GearPiece): string =
      let formatFirst f =
        match f with
        | first :: _ -> sprintf "%A" first
        | _ -> ""

      sprintf "  {Slot=%A; Id=%d; Ench=%s}" g.SlotId g.ItemId (formatFirst g.Enchants)

    let formatSlotGear (slotItems: PlayerItemsPerSlot): string =
      slotItems.Values
      |> Seq.toList
      |> List.map formatItem
      |> String.concat "\n"

    let formatAllSlotsGear (allSlots: AllEquipmentBySlot): string =
      allSlots.Values
      |> Seq.toList
      |> List.map formatSlotGear
      |> String.concat "\n"

    (sprintf "------ %A ------\n" e.Player)
    + (formatAllSlotsGear e.Gear)

  // Given combatant info, move all gear from that info to the player's recorded gear history
  let updateGearSlots (ci: CombatantInfo, reportItem: EnchantReportItem) =
    for gearPiece in ci.Equipment do
      if isSlotEnchantable gearPiece.SlotId then
        if reportItem.Gear.ContainsKey gearPiece.SlotId = false
        then reportItem.Gear.Add(gearPiece.SlotId, PlayerItemsPerSlot())
        let dictToUpdate = reportItem.Gear.[gearPiece.SlotId]

        if dictToUpdate.ContainsKey gearPiece.ItemId = false
        then dictToUpdate.Add(gearPiece.ItemId, gearPiece)

  let scanEnchants (raid: RaidState, events: CombatLogEvent list): EnchantReportItem list =
    let result = List<EnchantReportItem>()

    for player in raid.Players.Values do
      let reportItem: EnchantReportItem =
        { Player = player
          Gear = AllEquipmentBySlot() }

      // For each player filter events and gather all gear that player had equipped
      for ev in events do
        match ev with
        | CombatLogEvent.CombatantInfo ci when isSamePlayer (raid, ci.Player, player) ->
            updateGearSlots (ci, reportItem)
        | _ -> ()

      result.Add reportItem

    result |> Seq.toList
