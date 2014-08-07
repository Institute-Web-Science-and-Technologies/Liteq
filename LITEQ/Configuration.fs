module Configuration

// TODO: (besonders DICK!!!) - wie gehen wir mit mehren Configs um?

open System
open System.IO

let KEY_SERVER_URI = "serverUri"
let KEY_UPDATE_URI = "updateUri"
let KEY_SCHEMA_FILE = "schemaFile"
let KEY_GATHER_PREFIX_DATA = "gatherPrefixData"
let KEY_PREFIX_FILE = "prefixFile"
let KEY_LOG_LEVEL = "logLevel"

let DEFAULT_LOG_LEVEL = 5

let internal logger = Logger.Logger(DEFAULT_LOG_LEVEL)

type Configuration(fileName:string) = 
    let mutable conf : (string * string) list = List.empty
    let mutable prefixes : (string * string) list = List.empty

    let hasConfVal key = 
                not(conf.IsEmpty) && 
                conf |> List.exists (fun (k, v) -> k = key)

    let findConfVal key = 
                conf |> List.pick (fun (k, v) -> 
                            if k = key then Some(v)
                            else None)

    let initConf = 
        conf <- [ for line in File.ReadAllLines(fileName) do
                      if not (line.StartsWith("//")) then yield (line.Split('=') |> (fun a -> a.[0].Trim(), a.[1].Trim())) ]
        if hasConfVal "prefixFile" then 
            prefixes <- [ for line in File.ReadAllLines(findConfVal ("prefixFile")) do
                              yield (line.Split('=') |> (fun a -> a.[0].Trim(), a.[1].Trim())) ]
        else logger.Info "No prefix file defined"

    do initConf

    member this.FindConfValue (key : string) = findConfVal key
    member this.HasConfValue (key : string) = hasConfVal key
    member this.Prefixes = prefixes
            



