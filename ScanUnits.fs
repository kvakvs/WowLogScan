namespace WowLogScan

module ScanUnits =
  open System.Collections.Generic
  open RaidState
  open EventLog
  open Model.Unit

  let updateDict (players: Dictionary<string, Unit>, v: string []) =
    let unitId = v.[1]
    if unitId.StartsWith "Player-"
       && players.ContainsKey(unitId) = false then
      players.Add(unitId, createUnit v.[2])

  // Go through the event log and try map some unit GUIDs to names
  let scanUnits (logLines: LogLine list): RaidState =
    let players = Dictionary<string, Unit>()

    for line in logLines do
      let v = line.Values
      match v.[0] with
      | x when x.StartsWith("SPELL_") -> updateDict (players, v)
      | _ -> ()

    { Players = players }
