namespace WowLogScan

module ScanBuffs =
  open System.Collections.Generic
  open WowLogScan.EventLog
  open WowLogScan.Model.Unit
  open Buffs
  open RaidState

  // Records a gained or lost buff
  type FoundBuff = { Player: Unit; Buff: WorldBuff }

  // Return mapping of player to world buffs they had in this log
  let scanWorldBuffs (raid: RaidState, events: CombatLogEvent list) =
    // TODO: Combine lost buffs info with COMBATANT_INFO
    let allFoundBuffs = List<FoundBuff>()

    for ev in events do
      match ev with
      | CombatLogEvent.Spell sp when sp.Prefix = SpellPrefix.Spell ->
          match sp.Spell with
          | Ability.Spell spellName ->
              match recognizeBuff spellName with
              | None -> ()
              | Some buff ->
                  let foundBuff = { Player = sp.Target; Buff = buff }
                  // printfn "Found buff %+A" foundBuff
                  allFoundBuffs.Add(foundBuff)
          | _ -> ()
      | _ -> ()

    let result = Dictionary<Unit, Set<WorldBuff>>()
    
    for foundBuff in allFoundBuffs do
      if result.ContainsKey(foundBuff.Player) = false then
        result.Add(foundBuff.Player, Set<WorldBuff>([foundBuff.Buff]))
      else
        let upd = result.[foundBuff.Player].Add(foundBuff.Buff)
        result.Remove(foundBuff.Player) |> ignore
        result.Add(foundBuff.Player, upd)
      
    result
