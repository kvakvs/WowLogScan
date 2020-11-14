namespace WowLogScan

module RaidState =
  open System.Collections.Generic
  open TargetType

  type RaidState = { Players: Dictionary<string, TargetType.Unit> }

  let resolvePlayer (raid: RaidState, u: Unit): Unit =
    match u with
    | Unit.PlayerId id -> raid.Players.[id]
    | _ -> u

  let isSamePlayer (raid: RaidState, a: Unit, b: Unit): bool =
    resolvePlayer(raid, a) = resolvePlayer(raid, b)
