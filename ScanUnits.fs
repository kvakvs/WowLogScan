namespace WowLogScan


module ScanUnits =
  open WowLogScan.CombatlogToken
  open System.Collections.Generic
  open RaidState
  open Target

  let updateDict (players: Dictionary<string, TargetType.Unit>, v: CLToken []) =
    match Target.unitFromToken v.[1] with
    | TargetType.Unit.PlayerId p when players.ContainsKey(p) = false ->
      players.Add(p, Target.unitFromToken v.[2])
    | _ -> ()

  // Go through the event log and try map some unit GUIDs to names
  let scanUnits (logLines: CLEvent list): RaidState =
    let players = Dictionary<string, TargetType.Unit>()

    for line in logLines do
      let v = line.Args

      match v.[0] with
      | CLToken.String x when x.StartsWith("SPELL_") -> updateDict (players, v)
      | _ -> ()

    { Players = players }
