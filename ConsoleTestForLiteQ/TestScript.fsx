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


#r @"..\LiteqTypeProvider\bin\Debug\LiteQTypeProvider.dll"
#r @"..\LiteqTypeProvider\bin\Debug\dotNetRDF.dll"

open Uniko.West.LITEQTypeProvider
open System.Collections.Generic

let dataContext = Uniko.West.LITEQTypeProvider.RdfNavigation<"http://stardog.west.uni-koblenz.de:8080/openrdf-sesame", "test", 100>.GetDataContext()

// navigation to a class
let classNav = dataContext.``http://example.org/ns#creature``.SubTypeNavigation.``http://example.org/ns#person``

let ext = classNav.PropRestriction.``http://example.org/ns#owns``.Extension

let personsWithName = classNav.PropRestriction.``http://example.org/ns#hasName``.PropRestriction.``http://example.org/ns#owns``.Individuals


let pclass = classNav.Individuals.``http://example.org/ns#heinz``.getRdfObject.GetType()

let heinz = classNav.Individuals.``http://example.org/ns#heinz``.getRdfObject
printfn "%s" ( heinz.get_hasName()|> String.concat " " )

// getting a class object
let personClass = classNav.Intension

let pCreator = personClass.GetMember("create")


// getting individual RDF objects of a class
let individualObjs = classNav.Extension
printfn "%s" (individualObjs |> List.map (fun x -> (x.get_hasName() |> Seq.map (fun y -> y.ToString()) |> String.concat("; ")) ) |> String.concat(", "))

//fails because of empty input sequence!
//printfn "%s" (individualObjs |> List.map (fun x -> (x.get_owns() |> Seq.map (fun y -> y.ID.ToString()) |> String.concat("; ")) ) |> String.concat(", "))

let heinz = individualObjs.Head

//heinz.replace_hasAge(["23"])

(*let mutable nameSet = heinz.get_hasName().GetEnumerator()
nameSet.MoveNext()
printfn "My name is %s" (nameSet.Current)

let heinzAge = ["13"]
heinz.add_hasAge(heinzAge)

let mutable ageSet = heinz.get_hasAge().GetEnumerator()
ageSet.MoveNext()
printfn "My age is %s" (ageSet.Current)
*)

// navigation to an individual
let indNav = classNav.Individuals.``http://example.org/ns#stefan``

// getting the individual's RDF object
let ind = indNav.getRdfObject
