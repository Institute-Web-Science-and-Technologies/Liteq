module Configuration

open System
open System.IO


let mutable internal conf:(string*string) list = List.empty
let mutable internal prefixes:(string*string) list = List.empty


let findConfVal(key:string) =
    conf |> List.pick(fun (k,v) -> if k = key then Some(v) else None)

let hasConfVal(key:string) =
    conf |> List.exists( fun (k,v) -> k = key )

let initConf(filename:string) = 
    conf <- [ for line in File.ReadAllLines(filename) do
              if not(line.StartsWith("//")) then yield (line.Split('=') |> (fun a -> a.[0].Trim(), a.[1].Trim())) ]
    prefixes <- [ for line in File.ReadAllLines(findConfVal("prefixfile")) do
                  yield (line.Split('=') |> (fun a -> a.[0].Trim(), a.[1].Trim())) ]


//    printfn "%A" (readIni("liteq_config.txt"))
//    printfn "%A" (getVal("key1"))
//    printfn "%A" (getVal("key2"))
//    Console.ReadLine() |> ignore

