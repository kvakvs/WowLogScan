namespace WowLogScan

open System

module EffortScore =
  open WowLogScan.Buffs

  // Give worldbuffs a DKP score
  // Rend is ignored
  // First 2 world buffs give 3 DKP
  // Each consequent buffs add 1 DKP to max 5
  let scoreWorldBuffs (b: Set<WorldBuff>): int =
    let buffList = b |> Seq.toList |> List.filter (fun b -> b <> WorldBuff.Rend)

    match List.length buffList with
      | n when n < 2 -> 0
      | n -> Math.Min(5, (n - 2) + 3)
      