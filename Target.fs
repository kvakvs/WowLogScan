﻿namespace WowLogScan

open WowLogScan.ProgramState

module Target =
  open WowLogScan.CombatlogToken
  open TargetType
  
  let createUnit (s: string): Unit =
    if s = "nil" then
      Unit.NoTarget
    else if s.Contains(Global.RealmName) then
      let name = (s.Split '-').[0] // trim "" and take Name from Name-HydraxianWaterlords
      Unit.Player name
    else
      Unit.Npc s

  let unitFromToken (t: CLToken): Unit =
    match t with
    | CLToken.UnitId s ->
      match s with
      | s when s.StartsWith("Player-") -> Unit.PlayerId s
      | _ -> Unit.Npc s
    | CLToken.Player s -> Unit.Player s
    | CLToken.Nil | CLToken.Int64 0L -> Unit.NoTarget
    | CLToken.String s -> Unit.Pet s
    | _ -> failwithf "UnitID or Player token expected, instead got %A" t

  let playerName (u: Unit): string =
    match u with
    | Player s -> s
    | _ -> failwithf "Expected Player(_) and got %A" u
    