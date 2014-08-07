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

let internal logger = Logger.Logger(5)
let mutable internal conf : (string * string) list = List.empty
let mutable internal prefixes : (string * string) list = List.empty

let findConfVal (key : string) = 
    conf |> List.pick (fun (k, v) -> 
                if k = key then Some(v)
                else None)

let hasConfVal (key : string) = conf |> List.exists (fun (k, v) -> k = key)

let initConf (filename : string) = 
    conf <- [ for line in File.ReadAllLines(filename) do
                  if not (line.StartsWith("//")) then yield (line.Split('=') |> (fun a -> a.[0].Trim(), a.[1].Trim())) ]
    if hasConfVal "prefixFile" then 
        prefixes <- [ for line in File.ReadAllLines(findConfVal ("prefixFile")) do
                          yield (line.Split('=') |> (fun a -> a.[0].Trim(), a.[1].Trim())) ]
    else logger.Info "No prefix file defined"
//    printfn "%A" (readIni("liteq_config.txt"))
//    printfn "%A" (getVal("key1"))
//    printfn "%A" (getVal("key2"))
//    Console.ReadLine() |> ignore