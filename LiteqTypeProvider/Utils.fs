(*
Copyright 2014 Stefan Scheglmann

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*)


namespace Uniko.West.LITEQTypeProvider

open System.Diagnostics
open System.Text.RegularExpressions

module Utils =
    
    let castAs<'T when 'T : null> (o:obj) =
        match o with
        | :? 'T as res -> res
        | _ -> null

    // BUGGED! test that first
    let cutLiteralFormat (litWithFormat:string) =
        litWithFormat.Substring(0, litWithFormat.IndexOf("^^"))

    let generateGetterSetterForProp(propUri:string, prefix:string)=
        System.Text.RegularExpressions.Regex.Replace(propUri, ".*#", prefix+"_")
                
    // loglevel: (0=Off|1=Error|2=Warning|3=Info|4=Verbose)
    let lqLoglvl = TraceSwitch("lqDebugLevel", "configures Liteq TypeProvider Debug outputs", "4")
    let cacheLoglvl = TraceSwitch("cacheDebugLevel", "configures Cache Debug outputs", "0")
    let conLoglvl = TraceSwitch("conDebugLevel", "configures RDF Connection Debug outputs", "0")
    let typeLoglvl = TraceSwitch("typeDebugLevel", "configures RDFTypes Debug outputs", "0")
