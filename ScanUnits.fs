namespace WowLogScan


module ScanUnits =
  open WowLogScan.CombatlogType
  open System.Collections.Generic
  open RaidState
  open Model.Unit

  let updateDict (players: Dictionary<string, Unit>, v: CLToken []) =
    match unitFromToken v.[1] with
    | Unit.PlayerId p when players.ContainsKey(p) = false -> players.Add(p, unitFromToken v.[2])
    | _ -> ()

  // Go through the event log and try map some unit GUIDs to names
  let scanUnits (logLines: CLEvent list): RaidState =
    let players = Dictionary<string, Unit>()

    for line in logLines do
      let v = line.Args

      match v.[0] with
      | CLToken.String x when x.StartsWith("SPELL_") -> updateDict (players, v)
      | _ -> ()

    { Players = players }
