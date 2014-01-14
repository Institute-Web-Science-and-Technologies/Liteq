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

open System
open System.Text
open Microsoft.FSharp.Reflection
open System.Collections.Generic

module tools = 
    let toString (x:'a) = 
        match FSharpValue.GetUnionFields(x, typeof<'a>) with
        | case, _ -> case.Name
    
type NavNodeType =
    | Undefined
    //RDF Navnodes
    | RdfClass
    | RdfIndividual
    | RdfLiteral
    | RdfProperty
    //relation (dummy) navnodes
    | SubTypeNav
    | SuperTypeNav
    | IndividualsNav
    | PropertyNav
    | PropertyRes
    // exit navnodes
//    | Intension   not needed since it returns typeof<X> directly
    | Extension
    | IndividualObj
    with
    member this.toString = tools.toString this