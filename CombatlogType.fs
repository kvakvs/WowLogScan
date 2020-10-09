namespace WowLogScan


module CombatlogType =
  open System

  type CLDate = CLDate of string

  type EnvDamageType = Falling

  // A parsed item in combat log line: string, number or a structure
  type CLToken =
    | Time of DateTime
    | EventName of string
    | String of string
    | UnitId of string
    | Player of string
    | Version of string // version string 1.13.2
    | Int64 of int64
    | Float of float
    | Nil
    | List of CLToken list
    | Environmental of EnvDamageType

  type CLEvent =
    { Time: DateTime
      Name: string
      Args: CLToken [] }

  let extractString (t: CLToken): string =
    match t with
    | CLToken.String s -> s
    | _ -> failwithf "String token expected, instead got %A" t

  let extractInt (t: CLToken): int64 =
    match t with
    | CLToken.Int64 i -> i
    | _ -> failwithf "Int64 token expected, instead got %A" t
