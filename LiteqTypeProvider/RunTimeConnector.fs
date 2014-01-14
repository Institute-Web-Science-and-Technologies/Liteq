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
open System.Diagnostics
open System.Collections.Generic
open VDS.RDF.Query
open VDS.RDF.Storage
open VDS.RDF.Parsing
open Uniko.West.LITEQTypeProvider.Utils

module RunTimeConnector = 
    
    type SesameHttpStoreConnection(baseUri:string, storeID:string) = 
        
        static let objectCache = new Dictionary<Uri,Object>()

        let con = new SesameHttpProtocolConnector(baseUri, storeID) 

        member private this.executeUpdate(queryString:string) =
            con.Update(queryString)

        member this.executeUpdate(queryString:SparqlParameterizedString) =
            this.addNSs(queryString)
            this.executeUpdate(queryString.ToString())

        member private this.executeQuery(queryString:string) = 
            con.Query(queryString) :?> SparqlResultSet

        member this.executeQuery(queryString:SparqlParameterizedString) =
            this.addNSs(queryString)
//            if memorizeQueries then 
//                Debug.WriteLineIf(Utils.conLoglvl.TraceInfo, "   Excuting queryStringMemorized: '" + queryString.ToString() + "'")
//                this.executeQueryMemorized this.executeQuery (queryString.ToString())
//            else 
            Debug.WriteLineIf(Utils.conLoglvl.TraceInfo, "   Excuting queryString: '" + queryString.ToString() + "'")
            this.executeQuery(queryString.ToString())

        member this.addNSs(queryString:SparqlParameterizedString) =
            queryString.Namespaces.AddNamespace("ex", new Uri("http://example.org/ns#"))
            queryString.Namespaces.AddNamespace("rdf", new Uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#"))
            queryString.Namespaces.AddNamespace("rdfs", new Uri("http://www.w3.org/2000/01/rdf-schema#"))

        member this.addNamespaces(queryString : SparqlParameterizedString, namespaces : Dictionary<string, Uri>) =
            namespaces |> Seq.iter (fun (KeyValue(k,v)) -> queryString.Namespaces.AddNamespace(k,v))

        member this.registerObject(id:Uri, rdfObject:Object) = objectCache.Add(id, rdfObject)

    type ConnectionFactory private() =
        static let connectionCache = new Dictionary<string*string,SesameHttpStoreConnection>()

        static let mutable instance = lazy (new ConnectionFactory())
        static member Instance with get() = instance.Value

        static member createConnection(baseUri:string, storeID:string) = 
            if not(connectionCache.ContainsKey(baseUri,storeID)) then connectionCache.Add((baseUri,storeID), new SesameHttpStoreConnection(baseUri, storeID))
            connectionCache.Item((baseUri,storeID))
            
    type RDFDeSerializer() =
        
        // add multiple object values to store
        static member addValues(id:string, prop:string, values:HashSet<string>, isLiteral:bool, storeUtil : SesameHttpStoreConnection) = 
//            let objs = values |> Seq.mapi (fun i _ -> "@obj" + i.ToString()) |> String.concat(", ")
            let queryString = new SparqlParameterizedString()
            queryString.CommandText <- "INSERT DATA { @subject @predicate " + (values |> Seq.mapi (fun i _ -> "@obj" + i.ToString()) |> String.concat(", ")) + ". }"
            queryString.SetUri("subject", Uri(id))
            queryString.SetUri("predicate", Uri(prop))
            match isLiteral with
            |false -> values |> Seq.iteri (fun i value -> queryString.SetUri("obj" + i.ToString(), new Uri(value)))
            |true -> values |> Seq.iteri (fun i value -> queryString.SetLiteral("obj" + i.ToString(), value))
            storeUtil.executeUpdate(queryString)
        
        // add propertyMap to store
        static member updateObjectInStore(id:string, literalPropertyMap: Dictionary<string,HashSet<string>>,  objectPropertyMap: Dictionary<string,HashSet<string>>, storeUtil : SesameHttpStoreConnection) = 
            literalPropertyMap |> Seq.iter (fun (KeyValue(k,v)) -> RDFDeSerializer.addValues(id.ToString(), k, v, true, storeUtil))
            objectPropertyMap |> Seq.iter (fun (KeyValue(k,v)) -> RDFDeSerializer.addValues(id.ToString(), k, v, false, storeUtil))                 

        // Get all properties for a given individual
        static member Properties(id:string, storeUtil : SesameHttpStoreConnection ) =
            let queryString = new SparqlParameterizedString()
            queryString.CommandText <- "SELECT DISTINCT ?property WHERE { @individual ?property ?obj. FILTER NOT EXISTS { FILTER (regex( str(?property), str(<http://www.openrdf.org/schema>) )) } } "
            queryString.SetUri("individual", Uri(id))
            storeUtil.executeQuery(queryString) |> Seq.map (fun res -> res.Value("property").ToString())
    
        // Get all values for a given prop and given individual
        static member Values(id:string, prop:string, storeUtil : SesameHttpStoreConnection ) =
            let queryString = new SparqlParameterizedString()
            queryString.CommandText <- "SELECT  DISTINCT ?value WHERE { @individual @property ?value. } "
            queryString.SetUri("individual", Uri(id))
            queryString.SetUri("property",Uri(prop))
            storeUtil.executeQuery(queryString) |> Seq.map (fun res -> (res.Value("value").NodeType,res.Value("value").ToString()))

        static member Clean(id:string, prop:string, storeUtil:SesameHttpStoreConnection ) = 
            let queryString = new SparqlParameterizedString()
            queryString.CommandText <- "DELETE WHERE { @individual @property ?value. }"
            queryString.SetUri("individual", Uri(id))
            queryString.SetUri("property", Uri(prop))
            storeUtil.executeUpdate(queryString)

        static member CleanAll(id:string, storeUtil:SesameHttpStoreConnection) = 
            let queryString = new SparqlParameterizedString()
            queryString.CommandText <- "DELETE WHERE { @individual ?property ?value. }"
            queryString.SetUri("individual", Uri(id))
            storeUtil.executeUpdate(queryString)

            //        static member constUpdatePatternForProperty(id : Uri, property : Uri, values : List<IRDFResource>) : string = 
//            let queryPattern = id.ToString() + " " + property.ToString() + " " + RDFSerializer.constObjString values 
//            Debug.WriteLineIf(Utils.conLoglvl.TraceInfo, "update pattern : " + queryPattern)
//            queryPattern
//        
//        static member constUpdatePatternForALLProperties(id : Uri, propertyMap : Dictionary<Uri,List<IRDFResource>>) : string = 
//            let queryPattern = propertyMap |> Seq.map (fun (KeyValue(k,v)) -> RDFSerializer.constUpdatePatternForProperty(id, k, v)) |> String.concat("/n") 
//            Debug.WriteLineIf(Utils.conLoglvl.TraceInfo, "update pattern : " + queryPattern)
//            queryPattern
    
    
//        static member serializeAllProperties(rdfObject : IRDFClass, storeUtil : SesameHttpStoreUtils) =
//            Debug.WriteLineIf(Utils.conLoglvl.TraceInfo, " Serializing all properties for " + rdfObject.ID.ToString())
//            let updateQuery = RDFSerializer.constUpdateQuery(RDFSerializer.constUpdatePatternForALLProperties(rdfObject.ID, rdfObject.Properties))
//            Debug.WriteLineIf(Utils.conLoglvl.TraceInfo, " Executing update query " + updateQuery)
//            storeUtil.executeUpdate(updateQuery)
//    
//        static member serializeProperty(rdfObject : IRDFClass, property : Uri, storeUtil : SesameHttpStoreUtils) =
//            Debug.WriteLineIf(Utils.conLoglvl.TraceInfo, " Serializing property " + property.ToString() + " for " + rdfObject.ID.ToString())
//            let updateQuery = RDFSerializer.constUpdateQuery(RDFSerializer.constUpdatePatternForProperty(rdfObject.ID, property, rdfObject.Properties.Item(property)))
//            Debug.WriteLineIf(Utils.conLoglvl.TraceInfo, " Executing update query " + updateQuery)
//            storeUtil.executeUpdate(updateQuery)

//
//        // add single Value to Store
//        static member addValue(id:string, prop:Uri, value:String, isLiteral:bool, storeUtil : SesameHttpStoreConnection) =
//            let queryString = new SparqlParameterizedString()
//            queryString.CommandText <- "INSERT DATA { @subject @predicate @object}"
//            queryString.SetUri("subject", Uri(id))
//            queryString.SetUri("predicate", Uri(prop))
//            match isLiteral with
//            |false -> queryString.SetUri("predicate", new Uri(value))
//            |true -> queryString.SetLiteral("predicate", value)
//            storeUtil.executeUpdate(queryString)
//
//                    //
//        static member constObjString (objects : List<Uri>) : string = 
//            (objects |> Seq.map (fun o -> o.ToString()) |> String.concat(", ")) + " ."
//    
//        static member constValString (values : List<string>) : string =  
//            (values |> String.concat(", ")) + " ."
//
//        static member constUpdateQuery(queryString) : string =
//            "INSERT DATA { " + queryString + " }"