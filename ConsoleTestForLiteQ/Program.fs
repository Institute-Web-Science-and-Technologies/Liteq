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


open System
open System.Collections.Generic
open Uniko.West.LITEQTypeProvider
open Uniko.West.LITEQTypeProvider.RDFTypes
open Uniko.West.LITEQTypeProvider.RunTimeConnector

[<EntryPoint>]
let main argv = 
    
    let personUri = new Uri("http://example.org/ns#person")
    let creatureUri = new Uri("http://example.org/ns#creature")
    let animalUri = new Uri("http://example.org/ns#animal")
    let stefanUri = new Uri("http://example.org/ns#stefan")
    let lisaUri = new Uri("http://example.org/ns#lisa")
    let hasPetUri = new Uri("http://example.org/ns#owns")
    let baseUri = new Uri("http://www.w3.org/2000/01/rdf-schema#Resource")

    let sescon = SConnector("http://stardog.west.uni-koblenz.de:8080/openrdf-sesame", "test", false, 100)

//    Console.WriteLine("get direct properties for " + baseUri.ToString())
//    sescon.getAllPropsFT(baseUri) |> Seq.iter (fun res -> Console.WriteLine res)
//    Console.WriteLine("\n")
//
//    Console.WriteLine("Get direct properties (by schema) for individual " + stefanUri.ToString())
//    sescon.getPropsFIBS(stefanUri) |> Seq.iter (fun res -> Console.WriteLine res)
//    Console.WriteLine("\n")
//
//    Console.WriteLine("Get all types for individual " + stefanUri.ToString())
//    sescon.getTypesFI(stefanUri) |> Seq.iter (fun res -> Console.WriteLine res)
//    Console.WriteLine("\n")
//
//    Console.WriteLine("Get direct subtypes for " + creatureUri.ToString())
//    sescon.getSubtypesFT(creatureUri) |> Seq.iter (fun res -> Console.WriteLine res)
//    Console.WriteLine("\n")
//
//    Console.WriteLine("Get basetypes for graph")
//    sescon.getBaseTypesForGraph() |> Seq.iter (fun res -> Console.WriteLine res)
//    Console.WriteLine("\n")
//
//    Console.WriteLine("Get Vaules of property " + hasPetUri.ToString() + " for instance" + lisaUri.ToString())
//    let (b,values) = sescon.getValuesOfPropFI(lisaUri, hasPetUri) 
//    //Console.WriteLine b.ToString
//    values |> Seq.iter (fun value -> Console.WriteLine value)
//    Console.WriteLine("\n")

    Console.WriteLine("!!!! Runtime Connector Tests !!!!")

    
    Console.WriteLine(" !! Read Tests !!\n")

    let rtc = ConnectionFactory.createConnection("http://stardog.west.uni-koblenz.de:8080/openrdf-sesame", "test")

    Console.WriteLine("Getting all Properties")
    RDFDeSerializer.Properties("http://example.org/ns#martin", rtc) |> Seq.iter (fun s -> Console.WriteLine(s))


    RDFDeSerializer.Properties("http://example.org/ns#martin", rtc) |> Seq.iter (fun s -> Console.WriteLine("Property " + s + " = " + (RDFDeSerializer.Values("http://example.org/ns#martin", s, rtc) |> Seq.map (fun (k,v) -> k.ToString() + " : " + v.ToString()) |> String.concat(", ")) ))

    Console.WriteLine(" !! Write Tests !!\n")

    let name = HashSet<string>(["martin"])
    let age = HashSet<string>(["25"])
    let lpMap = new Dictionary<string, HashSet<string>>()
    do lpMap.Add("http://example.org/ns#hasName", name)
    do lpMap.Add("http://example.org/ns#hasAge", age)

    let owns = HashSet<string>(["http://example.org/ns#fritz"])
    let opMap = new Dictionary<string, HashSet<string>>()
    do opMap.Add("http://example.org/ns#owns", owns)
//    
//    RDFDeSerializer.updateObject(Uri("http://example.org/ns#martin"), lpMap, opMap, rtc)
//    RDFDeSerializer.CleanAll("http://example.org/ns#martin", rtc)


//    RDFDeSerializer.Clean("http://example.org/ns#martin", "http://example.org/ns#owns", rtc)
//    RDFDeSerializer.addValues("http://example.org/ns#martin", "http://example.org/ns#owns", owns, false, rtc)



    RDFDeSerializer.Properties("http://example.org/ns#martin", rtc) |> Seq.iter (fun s -> Console.WriteLine(s))
    RDFDeSerializer.Properties("http://example.org/ns#martin", rtc) |> Seq.iter (fun s -> Console.WriteLine("Property " + s + " = " + (RDFDeSerializer.Values("http://example.org/ns#martin", s, rtc) |> Seq.map (fun (k,v) -> k.ToString() + " : " + v.ToString()) |> String.concat(", ")) ))
    Console.WriteLine("\n!!!! Object Tests !!!!")

    let martin = RDFObject.create("http://example.org/ns#martin", "http://stardog.west.uni-koblenz.de:8080/openrdf-sesame", "test")
    let fritz = RDFObject.create("http://example.org/ns#fritz", "http://stardog.west.uni-koblenz.de:8080/openrdf-sesame", "test")
    let frotz = RDFObject.create("http://example.org/ns#frotz", "http://stardog.west.uni-koblenz.de:8080/openrdf-sesame", "test")
    let fratz = RDFObject.create("http://example.org/ns#fratz", "http://stardog.west.uni-koblenz.de:8080/openrdf-sesame", "test")
//    Console.WriteLine("Created " + martin.ID.ToString() )
//
//    Console.WriteLine("#literalproperties " + martin.LiteralProperties.Count.ToString() + " #objectProperties " + martin.ObjectProperties.Count.ToString())
//    do (martin :?> RDFObject).updateObject()
//    Console.WriteLine("#literalproperties " + martin.LiteralProperties.Count.ToString() + " #objectProperties " + martin.ObjectProperties.Count.ToString())
//
//    Console.WriteLine("\n")
//    do martin.LiteralProperties |> Seq.iter (fun (KeyValue(k,v)) -> Console.WriteLine(" Literal property : " + k.ToString() + " with values : " +  (v |> Seq.map (fun value -> value) |> String.concat", ")))
//    do martin.ObjectProperties |> Seq.iter (fun (KeyValue(k,v)) -> Console.WriteLine(" Object property : " + k.ToString() + " with values : " + (v |> Seq.map (fun value -> value.ID.ToString()) |> String.concat", ")))
//
//    (martin :?> RDFObject).RemoveValuesForProperty("http://example.org/ns#hasName")
//
//    Console.WriteLine("\n")
//    do martin.LiteralProperties |> Seq.iter (fun (KeyValue(k,v)) -> Console.WriteLine(" Literal property : " + k.ToString() + " with values : " +  (v |> Seq.map (fun value -> value) |> String.concat", ")))
//    do martin.ObjectProperties |> Seq.iter (fun (KeyValue(k,v)) -> Console.WriteLine(" Object property : " + k.ToString() + " with values : " + (v |> Seq.map (fun value -> value.ID.ToString()) |> String.concat", ")))
//
//    (martin :?> RDFObject).AddValuesToProperty("http://example.org/ns#hasName", name)
//    (martin :?> RDFObject).AddValuesToProperty("http://example.org/ns#owns", [fritz])
//    
//    Console.WriteLine("\n")
//    do martin.LiteralProperties |> Seq.iter (fun (KeyValue(k,v)) -> Console.WriteLine(" Literal property : " + k.ToString() + " with values : " +  (v |> Seq.map (fun value -> value) |> String.concat", ")))
//    do martin.ObjectProperties |> Seq.iter (fun (KeyValue(k,v)) -> Console.WriteLine(" Object property : " + k.ToString() + " with values : " + (v |> Seq.map (fun value -> value.ID.ToString()) |> String.concat", ")))
//
//    (martin :?> RDFObject).ReplaceValuesForProperty("http://example.org/ns#owns", [fratz])
//    
//    Console.WriteLine("\n")
//    do martin.LiteralProperties |> Seq.iter (fun (KeyValue(k,v)) -> Console.WriteLine(" Literal property : " + k.ToString() + " with values : " +  (v |> Seq.map (fun value -> value) |> String.concat", ")))
//    do martin.ObjectProperties |> Seq.iter (fun (KeyValue(k,v)) -> Console.WriteLine(" Object property : " + k.ToString() + " with values : " + (v |> Seq.map (fun value -> value.ID.ToString()) |> String.concat", ")))

    Console.WriteLine(fratz.GetHashCode())
    Console.WriteLine(fritz.GetHashCode())
    Console.WriteLine(frotz.GetHashCode())
    Console.WriteLine(frotz.GetType().GetHashCode())
    

    Console.ReadLine() |> ignore
    0 // return an integer exit code
