namespace WowLogScan
module TargetType =
    type Unit =
    | Player of string // player name, not id
    | PlayerId of string // player unit id, not name
    | Npc of string
    | Pet of string
    | NoTarget
