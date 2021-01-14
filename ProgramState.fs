namespace WowLogScan

module ProgramState =
  type Global() =
    static let mutable realmName: string = "MirageRaceway"
    
    static member RealmName 
        with get() = realmName
        and set v = realmName <- v
