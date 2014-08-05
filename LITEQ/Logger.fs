module Logger
 
open System.Diagnostics
 


type Logger(lvl:int) =
    
    // ALL=5 > DEBUG=4 > INFO=3 > WARN=2 > ERROR=1 > OFF=0
    member this.Debug(msg) = 
        System.Diagnostics.Debug.WriteLineIf(lvl>=4, "[DEBUG] " + msg)
    member this.Info(msg:string) = 
        System.Diagnostics.Debug.WriteLineIf(lvl>=3, "[INFO] " + msg)
    member this.Warn(msg:string) = 
        System.Diagnostics.Debug.WriteLineIf(lvl>=2, "[WARN] " + msg)
    member this.Error(msg:string) = 
        System.Diagnostics.Debug.WriteLineIf(lvl>=1, "[ERROR]" + msg)