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
open System.Collections.Generic
open System.Reflection
open System.Diagnostics
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open Uniko.West.LITEQTypeProvider.Utils
open Utils
open VDS.RDF.Query.Inference
open VDS.RDF.Query
open VDS.RDF.Parsing
open VDS.RDF
open VDS.RDF.Storage.Management
open VDS.RDF.Storage

[<AutoOpen>]
type SesameHttpStoreUtils(baseUri:string, storeID:string, memorizeQueries:bool) =
    let queryCache = new Dictionary<String, SparqlResultSet>(HashIdentity.Structural)

    let con = new SesameHttpProtocolConnector(baseUri, storeID)
    let queryParser = new SparqlQueryParser()
    let updateParser = new SparqlUpdateParser()
    //Debug.WriteLineIf(Utils.conLoglvl.TraceInfo,"Http connection to %s, repository %s established: %b", baseUri, storeID, con.IsReady)

    member this.baseUri with get() = baseUri
    member this.storeID with get() = storeID

    member this.executeQueryMemorized(executeFunction) =
        fun query -> 
                let key = query.ToString()
                match queryCache.TryGetValue key with
                | (true, results) ->
                    Debug.WriteLineIf(Utils.conLoglvl.TraceInfo, "             FROM CACHE!!!")
                    results
                | false, _ ->
                    let results = executeFunction(query)
                    queryCache.Add(key, results)
                    results
        
    member this.getConnection():SesameHttpProtocolConnector = con

    member this.executeUpdate(queryString:string) =
        con.Update(queryString)

    member this.executeQuery(queryString:string) = 
        con.Query(queryString) :?> SparqlResultSet

    member this.executeQueryString(queryString:SparqlParameterizedString) =
        this.addNSs(queryString)
        if memorizeQueries then 
            Debug.WriteLineIf(Utils.conLoglvl.TraceInfo, "   Excuting queryStringMemorized: '" + queryString.ToString() + "'")
            this.executeQueryMemorized this.executeQuery (queryString.ToString())
        else 
            Debug.WriteLineIf(Utils.conLoglvl.TraceInfo, "   Excuting queryString: '" + queryString.ToString() + "'")
            this.executeQuery(queryString.ToString())

    member this.addNSs(queryString:SparqlParameterizedString) =
        queryString.Namespaces.AddNamespace("ex", new Uri("http://example.org/ns#"))
        queryString.Namespaces.AddNamespace("rdf", new Uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#"))
        queryString.Namespaces.AddNamespace("rdfs", new Uri("http://www.w3.org/2000/01/rdf-schema#"))

    member this.addNamespaces(queryString : SparqlParameterizedString, namespaces : Dictionary<string, Uri>) =
        namespaces |> Seq.iter (fun (KeyValue(k,v)) -> queryString.Namespaces.AddNamespace(k,v))

[<AutoOpen>]
type SConnector(baseUri:String, storeID:String, useQueryCaching:bool, queryLimit:int) =
    do Debug.WriteLineIf(Utils.conLoglvl.TraceInfo, ("Initializing RDF connection to server: %s and repo %s", baseUri, storeID))

    let limit = queryLimit.ToString()

    let storeUtil = new SesameHttpStoreUtils(baseUri, storeID, useQueryCaching)

    let subClassOf = new Uri("http://www.w3.org/2000/01/rdf-schema#subClassOf")
    let rdfsClass = new Uri("http://www.w3.org/2000/01/rdf-schema#Class")
    let rdfsLiteral = new Uri("http://www.w3.org/2000/01/rdf-schema#Literal")
    let domain  = new Uri("http://www.w3.org/2000/01/rdf-schema#domain")
    let range  = new Uri("http://www.w3.org/2000/01/rdf-schema#range")
    let rdfsProperty = new Uri("http://www.w3.org/2000/01/rdf-schema#Property")
    let rdfType = new Uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
    let subPropertyOf = new Uri("http://www.w3.org/2000/01/rdf-schema#subPropertyOf")
    let rdfsResource = new Uri("http://www.w3.org/2000/01/rdf-schema#Resource")

    member this.getBaseTypesForGraph() =
        Debug.WriteLineIf(Utils.conLoglvl.TraceInfo, "getting all base types for graph ")
        let queryString = new SparqlParameterizedString()
        queryString.CommandText <- @"SELECT DISTINCT ?subtype WHERE { ?subtype @subClassOf @rdfsResource FILTER NOT EXISTS {FILTER (regex(str(?subtype), str(rdf:)) || regex(str(?subtype),str(rdfs:)))} FILTER NOT EXISTS {?subtype @subClassOf ?intertype. ?intertype @subClassOf @rdfsResource. FILTER (?intertype != ?subtype && ?intertype != @rdfsResource )}}  LIMIT " + limit //  
        queryString.SetUri("rdfsResource", rdfsResource)
        queryString.SetUri("subClassOf", subClassOf)
        queryString.SetUri("rdfsClass", rdfsClass)
        storeUtil.executeQueryString(queryString) |> Seq.map (fun res -> res.Value("subtype").ToString())

    // Get direct subtypes for a given type
    member this.getSubtypesFT(rdfType:Uri) = 
        Debug.WriteLineIf(Utils.conLoglvl.TraceInfo, "Getting direct subtypes of type " + rdfType.ToString())
        let queryString = new SparqlParameterizedString()
        queryString.CommandText <- "SELECT DISTINCT ?subtype WHERE { ?subtype @subClassOf @type. FILTER (?subtype != @type) FILTER NOT EXISTS { ?intertype @subClassOf @type. ?subtype @subClassOf ?intertype. FILTER(?intertype != ?subtype && ?intertype != @type && ?subtype != @type)}} LIMIT " + limit
        queryString.SetUri("type", rdfType) 
        queryString.SetUri("subClassOf", subClassOf)
        queryString.SetUri("class", rdfsClass) 
        storeUtil.executeQueryString(queryString) |> Seq.map (fun res -> res.Value("subtype").ToString())

    // Get all types for an individual
    member this.getTypesFI(rdfIndividual:Uri) = 
        Debug.WriteLineIf(Utils.conLoglvl.TraceInfo, "Getting types for individual  " + rdfIndividual.ToString())
        let queryString = new SparqlParameterizedString()
        queryString.CommandText <- "SELECT DISTINCT ?type WHERE { @individual @rdfType ?type FILTER NOT EXISTS {FILTER (regex(str(?type),str(rdfs:)))}}  LIMIT " + limit
        queryString.SetUri("individual", rdfIndividual)
        queryString.SetUri("rdfType", rdfType)
        storeUtil.executeQueryString(queryString) |> Seq.map (fun res -> res.Value("type").ToString())

    member this.isLiteral(range:string) = 
        range.Contains(rdfsLiteral.ToString())

    // gets all individuals of a given type uri
    member this.getIndividualsFT(typeUri:Uri, propRes: list<string>) =
        Debug.WriteLineIf(Utils.conLoglvl.TraceInfo, "getting all individuals of type " + typeUri.ToString() )
        let queryString = new SparqlParameterizedString()
        let propInfix = propRes |> Seq.mapi (fun i x -> "?ind <" + x + "> ?p"+i.ToString()+".") |> String.concat (" ")
        queryString.CommandText <- "SELECT DISTINCT ?ind WHERE { ?ind @type @typeUri. " + propInfix + "}  LIMIT " + limit
        queryString.SetUri("type", rdfType)
        queryString.SetUri("typeUri", typeUri)
        storeUtil.executeQueryString(queryString) |> Seq.map (fun res -> res.Value("ind").ToString())

    // Get direct properties and ranges (by schema) for a given instance 
    member this.getPropsAndRangesFI(rdfIndividual:Uri) = 
        Debug.WriteLineIf(Utils.conLoglvl.TraceInfo, "Getting direct properties and their range types of individual " + rdfIndividual.ToString())
        let queryString = new SparqlParameterizedString()
        queryString.CommandText <- "SELECT DISTINCT ?prop ?range WHERE { @individual @rdfType ?type. ?prop @domain ?type . ?prop @range ?range FILTER NOT EXISTS {FILTER (regex(str(?type),str(rdfs:)) || regex(str(?type),str(rdf:))) }}  LIMIT " + limit
        queryString.SetUri("individual", rdfIndividual) 
        queryString.SetUri("property", rdfsProperty)
        queryString.SetUri("rdfType", rdfType)
        queryString.SetUri("domain", domain)
        queryString.SetUri("range", range)
        storeUtil.executeQueryString(queryString) |> Seq.map (fun res -> (res.Value("prop").ToString(), res.Value("range").ToString()))
    
    // Get all (incl. supertypes) properties and ranges for a given type
    member this.getAllPropsAndRangesFT(rdfType:Uri) = 
        Debug.WriteLineIf(Utils.conLoglvl.TraceInfo, "Getting all properties of type " + rdfType.ToString())
        let queryString = new SparqlParameterizedString()
        queryString.CommandText <- "SELECT DISTINCT ?prop ?range WHERE { ?prop a @property. @type @subClassOf ?supertype. ?prop @domain ?supertype. ?prop @range ?range. }  LIMIT " + limit
        queryString.SetUri("type", rdfType) 
        queryString.SetUri("domain", domain)
        queryString.SetUri("range", range)
        queryString.SetUri("subClassOf", subClassOf)
        queryString.SetUri("property", rdfsProperty)
        storeUtil.executeQueryString(queryString) |> Seq.map (fun res -> (res.Value("prop").ToString(), res.Value("range").ToString()) )
    
    // Get values of direct properties for a given instance (+ literal indicator)
    member this.getValuesOfPropFI(indUri:Uri, propUri:Uri) = 
        Debug.WriteLineIf(Utils.conLoglvl.TraceInfo, "Getting direct properties (by schema) and their values for individual " + indUri.ToString())
        let queryString = new SparqlParameterizedString()
        queryString.CommandText <- "SELECT DISTINCT ?value WHERE { @individual @propUri ?value }  LIMIT " + limit
        queryString.SetUri("individual", indUri) 
        queryString.SetUri("propUri", propUri) 
        queryString.SetUri("property", rdfsProperty)
        queryString.SetUri("rdfType", rdfType)
        queryString.SetUri("domain", domain)
        let resultSet = storeUtil.executeQueryString(queryString)
        let isLiteral = 
            if (resultSet.IsEmpty) then false
            else
                resultSet.Item(0).Value("value").NodeType.Equals(NodeType.Literal)
        (isLiteral, resultSet |> Seq.map (fun res -> res.Value("value").ToString()))

    // Get ranges of direct properties (as tuple (property, range)) types for a given type and prop
    member this.getRangeFTAP(rdfType:Uri, rdfProp:Uri) =
        Debug.WriteLineIf(Utils.conLoglvl.TraceInfo, "Getting direct properties and their range type of type " + rdfType.ToString())
        let queryString = new SparqlParameterizedString()
        queryString.CommandText <- "SELECT DISTINCT ?range WHERE { @propUri a @property. @propUri @domain @type. @propUri @range ?range }  LIMIT " + limit
        queryString.SetUri("type", rdfType)
        queryString.SetUri("propUri", rdfProp)
        queryString.SetUri("domain", domain)
        queryString.SetUri("range", range)
        queryString.SetUri("property", rdfsProperty)
        storeUtil.executeQueryString(queryString) |> Seq.map (fun res -> (this.isRDFSLiteral(new Uri(res.Value("range").ToString())), res.Value("range").ToString()))

    // Check if rangetype is rdfs:Literal
    member this.isRDFSLiteral(rdftype:Uri) =
        rdftype.Equals rdfsLiteral

