namespace WowLogScan

module RaidState =
  open System.Collections.Generic
  open WowLogScan.EventLog

  type RaidState = {
      Players: Dictionary<string, Unit>
    }