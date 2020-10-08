namespace WowLogScan

module Buffs =
  type WorldBuff =
    | Dragonslayer | Rend | Zandalar | OgreKing | Songflower | DarkmoonFaire

  let recognizeBuff (s: string): Option<WorldBuff> =
    match s with
    | "Rallying Cry of the Dragonslayer" -> Some WorldBuff.Dragonslayer
    | "Warchief's Blessing" -> Some WorldBuff.Rend
    | "Spirit of Zandalar" -> Some WorldBuff.Zandalar
    | "Songflower Serenade" -> Some WorldBuff.Songflower
    | "Fengus' Ferocity" -> Some WorldBuff.OgreKing
    | "Mol'dar's Moxie" -> Some WorldBuff.OgreKing
    | "Slip'kik's Savvy" -> Some WorldBuff.OgreKing
    | s when s.Contains("Sayge's Dark Fortune") -> Some WorldBuff.DarkmoonFaire
    | _ -> None
