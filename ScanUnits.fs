namespace WowLogScan

module ScanUnits =
  open System.Collections.Generic
  open RaidState
  open EventLog

  let scanUnits (events: Event list): RaidState =
    let players = Dictionary<string, Unit>()

    { Players = players }
