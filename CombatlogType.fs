namespace WowLogScan


module CombatlogType =
  open System

  type CLDate = CLDate of string

  // A parsed item in combat log line: string, number or a structure
  type CLToken =
    | Time of DateTime
    | EventName of string
    | String of string
    | UnitId of string
    | Version of string // version string 1.13.2
    | Int of int64
    | Float of float
    | Nil
    | List of CLToken list

  type CLEvent =
    { Time: DateTime
      Name: string
      Args: CLToken list }
