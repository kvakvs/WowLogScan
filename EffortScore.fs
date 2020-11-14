namespace WowLogScan

open System

module EffortScore =
  open WowLogScan.Buffs

  // Give worldbuffs a DKP score
  // Rend is ignored
  // First 2 world buffs give 3 DKP
  // Each consequent buffs add 1 DKP to max 5
  let scoreWorldBuffs (b: Set<WorldBuff>): int =
    let filterOut value = fun b -> b <> value

    let buffList =
      b
      |> Seq.toList
      |> List.filter (filterOut WorldBuff.Rend)
      |> List.filter (filterOut WorldBuff.Songflower)

    match List.length buffList with
    | n when n < 2 -> 0
    | n -> Math.Min(5, (n - 2) + 3)
