namespace WowLogScan

module RaidState =
  open System.Collections.Generic
  open Model.Unit

  type RaidState = {
      Players: Dictionary<string, Unit>
    }