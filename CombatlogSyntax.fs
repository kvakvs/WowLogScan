namespace WowLogScan


module CombatlogSyntax =
  open System
  open FParsec
  open CombatlogType

  type ParserState = unit
  type P<'T> = Parser<'T, ParserState>

  let pvalue, pvalueRef =
    createParserForwardedToRef<CLToken, ParserState> ()

  // SPELL_PERIODIC_DAMAGE - identifier of event
  let identifierCaps: P<string> = many1Chars (upper <|> pchar '_') 
  
  let versionString: P<string> =
    let makeVersion (s: string list): string = s |> String.concat "."

    sepBy1 (many1Chars digit) (pchar '.')
    >>= (fun (lst: string list) ->
           if lst.Length = 3 then preturn lst
           else fail "Version must have 3 components")
    |>> makeVersion 

  let charsToString (c: char list): string = c |> List.toArray |> System.String
  
  let unitId: P<CLToken> =
    // Take (string, [[char, ...], ...]) and return string
    let makeUnitId (s: string, chars: char list list): CLToken =
      let segments = chars |> List.concat |> charsToString
      CLToken.UnitId (s + segments)
    
    choice [ pstring "Player-"
             pstring "Creature-"
             pstring "Pet-" ]
    .>>. sepBy1 (many1 hex) (pchar '-')
    |>> makeUnitId
    
  // let quotedUnitId = pchar '"' >>. unitId .>> pchar '"'

  let digitCharToInt (c: char): int64 = int64 c - int64 '0' 
  
  let quotedString: P<CLToken> =
    pchar '"' >>. manyTill anyChar (pchar '"')
    |>> (charsToString >> CLToken.String)
  
  let eventArg: P<CLToken> = choice [ unitId
                                      // attempt quotedUnitId
                                      identifierCaps |>> CLToken.String
                                      attempt versionString |>> CLToken.Version
                                      pfloat |>> CLToken.Float
                                      pint64 |>> CLToken.Int
                                      quotedString
                                      pstring "nil" >>% CLToken.Nil 
                                      ]

  let dateTime: P<DateTime> =
    // TODO: Optimize me, year does not change while parsing
    let addYear = "/" + DateTime.Today.Year.ToString()  + " "
    
    let makeDate (dayChars: char list, timeChars: char list) =
      let dstring = (dayChars |> charsToString) + addYear + (timeChars |> charsToString)
      printfn "%A" dstring
      DateTime.Parse dstring    

    // all till space then all till doublespace
    manyTill anyChar (pstring " ") .>>. manyTill anyChar (pstring "  ")
    |>> makeDate

  // Parses line of combat log with timestamp, event name and args
  let combatLogEvent: P<CLEvent> =
    let makeEvent (time: DateTime, args: CLToken list): CLEvent =
      match args.[0] with
      | CLToken.String name ->
        {Time = time
         Name = name 
         Args = args.[1..]}
      | _ -> failwith "Eventlog line must start with a date and an uppercase event name"
      
    dateTime .>>. sepBy eventArg (skipChar ',') .>> eof
    |>> makeEvent
