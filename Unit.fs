namespace WowLogScan.Model

module Unit =
  type Unit =
    | Player of string
    | Npc of string
    | NoTarget

  let REALM_NAME = @"HydraxianWaterlords"

  let createUnit (s: string): Unit =
    if s = "nil" then
      Unit.NoTarget
    else if s.Contains(REALM_NAME) then
      let name = (s.Split '-').[0] // trim "" and take Name from Name-HydraxianWaterlords
      Unit.Player name
    else
      Unit.Npc s
