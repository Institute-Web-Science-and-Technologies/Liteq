module TypeProviderImplementation.Configuration


open System
open System.IO

let KEY_SERVER_URI = "serverUri"
let KEY_UPDATE_URI = "updateUri"
let KEY_SCHEMA_FILE = "schemaFile"
let KEY_PREFIX_FILE = "prefixFile"

let DEFAULT_LOG_LEVEL = 5

let internal logger = Logger.Logger(DEFAULT_LOG_LEVEL)

type FileConfiguration(fileName:string) = 
    let mutable conf : (string * string) list = List.empty

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


    do initConf

    member this.FindConfValue (key : string) = if (hasConfVal key) then findConfVal key else ""
    member this.HasConfValue (key : string) = hasConfVal key


type Configuration =
    {   ServerUri : string
        UpdateUri : string
        SchemaFile : string
        PrefixFile : string
        Prefixes: (string*string) list }

    static member GetPrefixes (fileName:string) =
        [ for line in File.ReadAllLines(fileName) do
            yield (line.Split('=') |> (fun a -> a.[0].Trim(), a.[1].Trim())) ]
    
    member this.Validate() : unit =
        if  String.IsNullOrWhiteSpace this.ServerUri &&
            not ( System.Uri.IsWellFormedUriString(this.ServerUri, System.UriKind.Absolute) ) 
                then failwith "ServerUri was not found or empty"
        if  String.IsNullOrWhiteSpace this.SchemaFile
                then failwith "SchemaFile was not found or empty"
        
    static member CreateFromArgs (args : obj[]) =
        let configFile = args.[0] :?> string
        let serverUri = args.[1] :?> string
        let updateUri = args.[2] :?> string
        let schemaFile = args.[3] :?> string
        let prefixFile = args.[4] :?> string

        let conf : Configuration =
            if String.IsNullOrWhiteSpace serverUri 
                then Configuration.CreateFromFile ( configFile ) 
                else Configuration.Create (serverUri, schemaFile, updateUri, prefixFile)
        conf.Validate()
        conf

    static member CreateFromFile (fileName:string) =
        let fc = FileConfiguration(fileName)
        let conf = {    ServerUri = fc.FindConfValue KEY_SERVER_URI
                        UpdateUri = fc.FindConfValue KEY_UPDATE_URI
                        SchemaFile = fc.FindConfValue KEY_SCHEMA_FILE
                        PrefixFile = fc.FindConfValue KEY_PREFIX_FILE
                        Prefixes = if ( fc.FindConfValue KEY_PREFIX_FILE = "" ) 
                                    then List.empty
                                    else Configuration.GetPrefixes (fc.FindConfValue KEY_PREFIX_FILE) }
        conf.Validate()
        conf

    static member Create (serverUri, schemaFile, ?updateUri, ?prefixFile) =
        let updateUri = defaultArg updateUri ""
        let prefixFile = defaultArg prefixFile ""
        let conf = {    ServerUri = serverUri
                        UpdateUri = updateUri
                        SchemaFile = schemaFile
                        PrefixFile = prefixFile
                        Prefixes = if ( prefixFile = "" ) 
                                    then List.empty
                                    else Configuration.GetPrefixes ( prefixFile ) }
        conf.Validate()
        conf
        
            


            



