module TypeProviderImplementation.TypeMapper

open Wrapper

let private xsdMappings = 
    [ "http://www.w3.org/2001/XMLSchema#string", ("string", typeof<string>)
      "http://www.w3.org/2001/XMLSchema#int", ("integer", typeof<int>)
      "http://www.w3.org/2001/XMLSchema#integer", ("integer", typeof<int>)
      "http://www.w3.org/2001/XMLSchema#nonNegativeInteger", ("integer", typeof<int>)
      "http://www.w3.org/2001/XMLSchema#decimal", ("decimal", typeof<decimal>)
      "http://www.w3.org/2001/XMLSchema#float", ("float", typeof<float>)
      "http://www.w3.org/2001/XMLSchema#duration", ("string", typeof<string>)
      "http://www.w3.org/2001/XMLSchema#date", ("string", typeof<string>)
      "http://www.w3.org/2001/XMLSchema#dateTime", ("string", typeof<string>)
      "http://www.w3.org/2001/XMLSchema#gYear", ("string", typeof<string>)
      "http://www.w3.org/2001/XMLSchema#gMonth", ("string", typeof<string>)
      "http://www.w3.org/2001/XMLSchema#gYearMonth", ("string", typeof<string>)
      "http://www.w3.org/2001/XMLSchema#gDay", ("string", typeof<string>) ]
    |> dict

let mapPrimitiveType rangeUri = 
    if xsdMappings.ContainsKey rangeUri then xsdMappings.[rangeUri]
    else ("string", typeof<string>)

// TODO: Add missing XML datatypes
let internal mapPrimitive propertyUri rangeUri = 
    match rangeUri with
    | "http://www.w3.org/2001/XMLSchema#string" -> 
        typeof<string>, 
        (fun (args : Quotations.Expr list) -> 
            <@@ let wrapper = (%%args.[0] : obj) :?> RdfResourceWrapper
                accessProperty wrapper propertyUri @@>), 
        (fun (args : Quotations.Expr list) -> 
            <@@ let wrapper = (%%args.[0] : obj) :?> RdfResourceWrapper
                let value = (%%args.[1] : string list)
                setProperty wrapper propertyUri value @@>)
    | "http://www.w3.org/2001/XMLSchema#int" | "http://www.w3.org/2001/XMLSchema#integer" | "http://www.w3.org/2001/XMLSchema#nonNegativeInteger" -> 
        typeof<int>, 
        (fun (args : Quotations.Expr list) -> 
            <@@ let wrapper = (%%args.[0] : obj) :?> RdfResourceWrapper
                accessProperty wrapper propertyUri :?> string list |> List.map int @@>), 
        (fun (args : Quotations.Expr list) -> 
            <@@ let wrapper = (%%args.[0] : obj) :?> RdfResourceWrapper
                let value = (%%args.[1] : int list) |> List.map string
                setProperty wrapper propertyUri value @@>)
    | "http://www.w3.org/2001/XMLSchema#float" -> 
        typeof<float>, 
        (fun (args : Quotations.Expr list) -> 
            <@@ let wrapper = (%%args.[0] : obj) :?> RdfResourceWrapper
                accessProperty wrapper propertyUri :?> string list |> List.map float @@>), 
        (fun (args : Quotations.Expr list) -> 
            <@@ let wrapper = (%%args.[0] : obj) :?> RdfResourceWrapper
                let value = (%%args.[1] : float list) |> List.map string
                setProperty wrapper propertyUri value @@>)
    | "http://www.w3.org/2001/XMLSchema#decimal" -> 
        typeof<decimal>, 
        (fun (args : Quotations.Expr list) -> 
            <@@ let wrapper = (%%args.[0] : obj) :?> RdfResourceWrapper
                accessProperty wrapper propertyUri :?> string list |> List.map decimal @@>), 
        (fun (args : Quotations.Expr list) -> 
            <@@ let wrapper = (%%args.[0] : obj) :?> RdfResourceWrapper
                let value = (%%args.[1] : decimal list) |> List.map string
                setProperty wrapper propertyUri value @@>)
    | "http://www.w3.org/2000/01/rdf-schema#Literal" -> 
        typeof<string>, 
        (fun (args : Quotations.Expr list) -> 
            <@@ let wrapper = (%%args.[0] : obj) :?> RdfResourceWrapper
                accessProperty wrapper propertyUri @@>), 
        (fun (args : Quotations.Expr list) -> 
            <@@ let wrapper = (%%args.[0] : obj) :?> RdfResourceWrapper
                let value = (%%args.[1] : string list)
                setProperty wrapper propertyUri value @@>)
    | _ -> 
        typeof<string>, 
        (fun (args : Quotations.Expr list) -> 
            <@@ let wrapper = (%%args.[0] : obj) :?> RdfResourceWrapper
                accessProperty wrapper propertyUri @@>), 
        (fun (args : Quotations.Expr list) -> 
            <@@ let wrapper = (%%args.[0] : obj) :?> RdfResourceWrapper
                let value = (%%args.[1] : obj)
                setProperty wrapper propertyUri value @@>)

/// <summary>
/// Tests whether the given uri points to a primitive data type
/// </summary>
/// <param name="rangeUri">the URI to test</param>
let isPrimitive rangeUri = 
    if xsdMappings.ContainsKey rangeUri || rangeUri = "http://www.w3.org/2000/01/rdf-schema#Literal" then true
    else false
