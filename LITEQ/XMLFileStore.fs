module SomeName

open LITEQ
open Microsoft.FSharp.Linq
open System.Xml
open System.Xml.Linq
open System.Collections.Generic

// Namespace creating functions to simplify querying
let owl s = XName.Get(s, "http://www.w3.org/2002/07/owl#")
let rdf s = XName.Get(s, "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
let rdfs s = XName.Get(s, "http://www.w3.org/2000/01/rdf-schema#")
let xml s = XName.Get(s, XNamespace.Xml.NamespaceName)

/// <summary>Receives an RDF/XML element describing an class or property and returns a
/// proper name for this element</summary>
/// <param name="clss">The element describing the class or property</param>
let makeName (clss:XElement) : string =
    clss.Attribute(rdf "about").Value 

/// <summary>Receives an RDF/XML element describing an class or property and returns a
/// proper comment for this element</summary>
/// <param name=clss>The element describing the class or property</param>
let makeComment (clss:XElement) = 
    try
        "<summary>" +
            (clss.Elements(rdfs "comment") |> Seq.find( fun comment ->
                not (comment.Attribute(xml "lang") = null) 
                    || comment.Attribute(xml "lang").Value = "en"
            )).Value
        + "</summary>"
    with
        | :? KeyNotFoundException -> "<summary>No documentation available</summary>"

(*
 * TODO: This is way to slow...
 *
 *  Special cases that are included in this code:
 *  - properties without domain or range are ignored
 *  - Type owl:Thing is added manually
 *)

type XMLFileStore(filePath:string) as __ = class
    let document = 
        XDocument.Load(filePath)
    
    let root = document.Element(rdf "RDF")
    let superTypes = System.Collections.Generic.Dictionary<string, Set<string>>()
    let derivedTypes = System.Collections.Generic.Dictionary<string, (string*string) list>()
            
    /// List of all Class elements in ontology    
    let all_classes = 
        lazy 
            let classes = root.Elements(owl "Class")
            classes |> Seq.iter( fun clss ->
                let st =
                    new Set<string>(
                        query {
                            for st' in clss.Elements(rdfs "subClassOf") do
                                select (st'.Attribute(rdf "resource").Value)
                        })
                superTypes.Add (clss.Attribute(rdf "about").Value, st)

                for c in st do
                    if not (derivedTypes.ContainsKey c) then
                        derivedTypes.[c] <- []
                    derivedTypes.[c] <- (clss.Attribute(rdf "about").Value, makeName(clss)) 
                                            :: derivedTypes.[c]
            )
            superTypes.Add ("http://www.w3.org/2002/07/owl#Thing", Set.empty<string>) // see Header
            classes
    /// List of all properties in ontology
    let all_properties =
        lazy 
            [ for element in root.Elements() do
                if element.Name = (rdfs "Property") ||  element.Name = (owl "DatatypeProperty") 
                    || element.Name = (owl "ObjectProperty") then
                        if not (element.Element(rdfs "domain") = null) 
                            && not (element.Element(rdfs "range") = null) then
                                yield element ]

    /// <summary>Returns all superclasses (including the class itself) for a given class</summary>
    let superClassesForType (typeUri:string) : string list =  
        let rec helper(cls:string) : Set<string> =
            superTypes.[cls]
            |> Set.fold( fun (accumulator:Set<string>) (s:string) ->
                Set.union accumulator (helper s)
            ) (new Set<string>([cls]))

        Set.toList (helper typeUri)
        
    interface IStore with
        member __.Classes =
            query {
                for clss in all_classes.Force() do
                    select (clss.Attribute(rdf "about").Value, (makeName clss), (makeComment clss))
            } |> Seq.toList
            
        member __.Properties =
            query {
            for prop in all_properties.Force() do
                select (prop.Attribute(rdf "about").Value, (makeName prop), (makeComment prop), prop.Attribute(rdf "about").Value, prop.Attribute(rdf "about").Value)    
            } |> Seq.toList

                
        member __.PropertiesForType typeUri =
            let types = superClassesForType typeUri
            let includes (x:string) = Seq.exists (fun t -> t = x) types
            query {
                for prop in all_properties.Force() do
                    where (not(prop.Element(rdfs "domain") = null) && 
                            includes (prop.Element(rdfs "domain").Attribute(rdf "resource").Value) &&
                            not(prop.Element(rdfs "range") = null)) // see Header
                    select (prop.Attribute(rdf "about").Value, (makeName prop), (makeComment prop),
                                prop.Element(rdfs "range").Attribute(rdf "resource").Value)
            } |> Seq.toList
            

        member __.SubclassesForType typeUri = 
            if derivedTypes.ContainsKey typeUri 
                then derivedTypes.[typeUri]
                else []

        member __.RangeForProperty propertyUri =
            query {
                for prop in all_properties.Force() do 
                    where (prop.Attribute(rdf "about").Value = propertyUri)
                    select (prop.Element(rdfs "range").Attribute(rdf "resource").Value)
            } |> Seq.exactlyOne  
end
