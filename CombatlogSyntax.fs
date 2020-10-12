namespace WowLogScan

open System.Globalization


module CombatlogSyntax =
  open System
  open FParsec
  open CombatlogType

  let REALM_NAME = @"HydraxianWaterlords"

  type ParserState = unit
  type P<'T> = Parser<'T, ParserState>

  // SPELL_PERIODIC_DAMAGE - identifier of event
  let identifierCaps: P<string> = many1Chars (upper <|> pchar '_')

  let versionString: P<string> =
    let makeVersion (s: string list): string = s |> String.concat "."

    sepBy1 (many1Chars digit) (pchar '.')
    >>= (fun (lst: string list) -> if lst.Length = 3 then preturn lst else fail "Version must have 3 components")
    |>> makeVersion

  let charsToString (c: char list): string = c |> List.toArray |> System.String

  let unitId: P<CLToken> =
    // Take (string, [[char, ...], ...]) and return string
    let makeUnitId (s: string, chars: char list list): CLToken =
      let segments = chars |> List.concat |> charsToString
      CLToken.UnitId(s + segments)

    choice [ pstring "Player-"
             pstring "Creature-"
             pstring "GameObject-"
             pstring "Corpse-"
             pstring "Pet-" ]
    .>>. sepBy1 (many1 hex) (pchar '-')
    |>> makeUnitId

  let digitCharToInt (c: char): int64 = int64 c - int64 '0'

  let quotedString: P<CLToken> =
    let makeStringOrPlayer (s: string) =
      if s.EndsWith(REALM_NAME) then
        let name = (s.Split '-').[0]
        CLToken.Player name
      else
        CLToken.String s

    pchar '"'
    >>. manyTill anyChar (pchar '"')
    |>> (charsToString >> makeStringOrPlayer)

  let createHexToken (s: char list): CLToken =
    let str = s |> charsToString
    Int64.Parse(str, NumberStyles.HexNumber)
    |> CLToken.Int64
    
  let parseInt =
    pint64 .>> notFollowedByL (pchar '.') "Can't take integer when there seems to be a floating point"

  let damageKeyword =
    choice [ pstring "Falling"
             pstring "Fire" 
             pstring "Lava" ]
  
  let eventArg, eventArgImpl =
    createParserForwardedToRef<CLToken, ParserState> ()
  
  do eventArgImpl
     := choice [ damageKeyword
                 >>% CLToken.Environmental Falling
                 pstring "nil" >>% CLToken.Nil
                 pstring "0x" >>. many1 hex |>> createHexToken
                 unitId
                 pchar '(' >>. sepBy eventArg (skipChar ',') .>> pchar ')' |>> CLToken.List
                 pchar '[' >>. sepBy eventArg (skipChar ',') .>> pchar ']' |>> CLToken.List
                 identifierCaps |>> CLToken.String
                 attempt versionString |>> CLToken.Version
                 attempt parseInt |>> CLToken.Int64
                 pfloat |>> CLToken.Float 
                 quotedString ]

  let dateTime: P<DateTime> =
    // TODO: Optimize me, year does not change while parsing
    let addYear =
      "/" + DateTime.Today.Year.ToString() + " "

    let makeDate (dayChars: char list, timeChars: char list) =
      let dstring =
        (dayChars |> charsToString)
        + addYear
        + (timeChars |> charsToString)

      DateTime.Parse dstring

    // all till space then all till doublespace
    manyTill anyChar (pstring " ")
    .>>. manyTill anyChar (pstring "  ")
    |>> makeDate

  let makeEvent (time: DateTime, args: CLToken list): CLEvent =
    match args.[0] with
    | CLToken.String name ->
        { Time = time
          Name = name
          Args = args |> List.toArray }
    | _ -> failwith "Eventlog line must start with a date and an uppercase event name"

  // Parses line of combat log with timestamp, event name and args
  let parseCombatLogEvent: P<CLEvent> =
    dateTime
    .>>. sepBy eventArg (skipChar ',')
    |>> makeEvent

  let combatLogEvent: P<CLEvent> = parseCombatLogEvent .>> eof

  let combatLogEventList: P<CLEvent list> =
    many (parseCombatLogEvent .>> newline) .>> eof
