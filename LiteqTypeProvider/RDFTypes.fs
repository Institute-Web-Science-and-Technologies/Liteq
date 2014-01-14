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
open VDS.RDF
open Uniko.West.LITEQTypeProvider.RunTimeConnector
open Uniko.West.LITEQTypeProvider.Utils

module RDFTypes =
    
    type IRDFResource = 
        abstract Serialize : string

    type IRDFClass = 
        inherit IRDFResource
        abstract ID : Uri
//        abstract Properties : Dictionary<string,HashSet<IRDFResource>>
        abstract LiteralProperties : Dictionary<string,HashSet<string>>
        abstract ObjectProperties : Dictionary<string,HashSet<IRDFClass>>

    type IRDFLiteral<'T when 'T: equality> = 
        inherit IRDFResource
        abstract Value : 'T with get, set

    type RDFLiteral<'T when 'T: equality>(value : 'T) =
        let mutable v:'T = value

        override this.Equals(o) = 
            match o with
            | :? IRDFLiteral<'T> as other -> (this:>IRDFLiteral<'T>).Value.Equals(other.Value)
            | _ -> false
        
        override this.GetHashCode() = (this:>IRDFLiteral<'T>).Value.GetHashCode()

        interface IRDFLiteral<'T> with
            member this.Serialize = "Test"
            member this.Value with get() = v and set(value) = v <- value
                

    type RDFObject private(id:string, baseUri:string, storeID:string) =
        
        static let objectCache = new Dictionary<string,IRDFClass>()

//        let id = new Uri(id)
//        let mutable propertyMap = new Dictionary<string,HashSet<IRDFResource>>()

        let mutable objectPropertyMap = new Dictionary<string,HashSet<IRDFClass>>()
        let mutable literalPropertyMap = new Dictionary<string,HashSet<string>>()

        let con = ConnectionFactory.createConnection(baseUri, storeID)
        
        static member ObjectCache = objectCache
       
        static member create(id:string, baseUri:string, storeID:string) = 
            do Debug.WriteLineIf(Utils.typeLoglvl.TraceInfo, ("creating type " + id + " connected to " + baseUri + " store " + storeID))
//            do Console.WriteLine("Create called for: " + id + " connected to " + baseUri + " store " + storeID)
            if not(objectCache.ContainsKey(id)) then 
                do Debug.WriteLineIf(Utils.typeLoglvl.TraceInfo, ("Object not in cache! Creating new object with id: " + id + " connected to " + baseUri + " store " + storeID))
                do Console.WriteLine("Object not in cache! Creating new object with id: " + id + " connected to " + baseUri + " store " + storeID)
                objectCache.Add(id, new RDFObject(id, baseUri, storeID))
            objectCache.Item(id) 

        member private this._GetPropertyByURI(uri:string) =
            //Console.WriteLine("getting values for property " + uri.ToString())
            let values = RDFDeSerializer.Values(id, uri, con) 
            let isLiteral = match values |> Seq.head with 
                | NodeType.Literal,_ -> 
//                    Console.WriteLine("     isLiteral")
                    true
                | NodeType.Uri,_ -> 
//                    Console.WriteLine("     isUri")
                    false
            match (objectPropertyMap.ContainsKey(uri) || literalPropertyMap.ContainsKey(uri)) with
            | true -> RDFDeSerializer.Values(id, uri, con) |> Seq.iter (fun (nodeType,value) ->
                 if isLiteral then ignore(literalPropertyMap.Item(uri).Add(value.ToString()))
                 else ignore(objectPropertyMap.Item(uri).Add(RDFObject.create(value, baseUri, storeID))))
            | false -> 
                if isLiteral then literalPropertyMap.Add(uri, new HashSet<string>( values |> Seq.map (fun (_,value) -> value.ToString())))
                else objectPropertyMap.Add(uri,new HashSet<IRDFClass>( values |> Seq.map (fun (_,value) -> RDFObject.create(value, baseUri, storeID))))
            if isLiteral then literalPropertyMap.Item(uri) :> obj
            else objectPropertyMap.Item(uri) :> obj

//        member private this._GetPropertyByURI(uri:string) =
////            do Console.WriteLine("getting property " + uri.ToString())
//            do Debug.WriteLineIf(Utils.typeLoglvl.TraceInfo, ("getting property " + uri.ToString() )) 
//            match propertyMap.ContainsKey uri with
//            | true -> 
////                Console.WriteLine("      !Found! prop: " + uri.ToString())
//                propertyMap.Item(uri).UnionWith( RDFDeSerializer.Values(id, Uri(uri), con) |> Seq.map (fun (nodeType,value) ->
//                match nodeType with
//                | NodeType.Literal -> 
////                    do Console.WriteLine("      val: " + value + " ,NODETYPE : Literal")
//                    new RDFLiteral<String>(value.ToString()) :> IRDFResource
//                | NodeType.Uri -> 
////                    do Console.WriteLine("        val: " + value + " ,NODETYPE : Object")
//                    RDFObject.create(value.ToString(), baseUri, storeID) :> IRDFResource
//                ))
//            | false -> 
////                Console.WriteLine("      !NOT Found! prop: " + uri.ToString())
//                propertyMap.Add(uri, new HashSet<IRDFResource> (RDFDeSerializer.Values(id, Uri(uri), con) |> Seq.map (fun (nodeType,value) ->
//                match nodeType with
//                | NodeType.Literal -> 
////                    do Console.WriteLine("      val: " + value + " ,NODETYPE : Literal")
//                    new RDFLiteral<String>(value.ToString()) :> IRDFResource
//                | NodeType.Uri -> 
////                    do Console.WriteLine("        val: " + value + " ,NODETYPE : Object")
//                    RDFObject.create(value.ToString(), baseUri, storeID) :> IRDFResource
//                )))
//            propertyMap.Item(uri)
        
        member private this._RemoveObjectFromStore() = 
            RDFDeSerializer.CleanAll(id, con)

        member private this._RemoveValuesForProperty(uri) =
            if literalPropertyMap.ContainsKey(uri) then ignore(literalPropertyMap.Remove(uri))
            else if objectPropertyMap.ContainsKey(uri) then ignore(objectPropertyMap.Remove(uri))
            RDFDeSerializer.Clean(id, uri, con)

        member private this._AddValuesToProperty(uri, values:IEnumerable<IRDFClass>) = 
            Console.WriteLine("Try to add objects " + (values |> Seq.map (fun value -> value.ID.ToString()) |> String.concat(", ")))
          //  if objectPropertyMap.ContainsKey(uri) then Console.WriteLine(" Properties in objectPMap before: " + (objectPropertyMap.Item(uri) |> Seq.map (fun value -> value.ID.ToString()) |> String.concat(", ")))
            if not(objectPropertyMap.ContainsKey(uri)) then 
                Console.WriteLine("Property not found create new")
                objectPropertyMap.Add(uri, new HashSet<IRDFClass>())
            objectPropertyMap.Item(uri).UnionWith(values)
          //  Console.WriteLine(" Properties in objectPMap after: " + (objectPropertyMap.Item(uri) |> Seq.map (fun value -> value.ID.ToString()) |> String.concat(", ")))
            RDFDeSerializer.addValues(id, uri, new HashSet<string>(values |> Seq.map (fun value -> value.ID.ToString())), false, con)
        
        member private this._AddValuesToProperty(uri, values:IEnumerable<string>) = 
            Console.WriteLine("Try to add literals " + (values |> Seq.map (fun value -> value) |> String.concat(", ")))
            if not(literalPropertyMap.ContainsKey(uri)) then literalPropertyMap.Add(uri, new HashSet<string>())
            literalPropertyMap.Item(uri).UnionWith(values)
            RDFDeSerializer.addValues(id, uri, new HashSet<string>(values), true, con)
        
        member private this._ReplaceValuesForProperty(uri, values:IEnumerable<IRDFClass>) = 
            this._RemoveValuesForProperty uri
            this._AddValuesToProperty(uri, values)

        member private this._ReplaceValuesForProperty(uri, values:IEnumerable<string>) = 
            this._RemoveValuesForProperty uri
            this._AddValuesToProperty(uri, values)    
            
        member private this._UpdateObject() = 
            do Debug.WriteLineIf(Utils.typeLoglvl.TraceInfo, ("updating all properties ")) 
            RDFDeSerializer.Properties(id, con) |> Seq.iter (fun prop -> ignore(this._GetPropertyByURI(prop)))


        member private this._GetAllProperties() = 
//            do Console.WriteLine("getting all properties ")
            do Debug.WriteLineIf(Utils.typeLoglvl.TraceInfo, ("getting all properties ")) 
            RDFDeSerializer.Properties(id, con) |> Seq.iter (fun prop -> ignore(this._GetPropertyByURI(prop)))
            (objectPropertyMap,literalPropertyMap)

//        member this.SetPropertyByURI(uri, value:IRDFResource) = 
//           if not(propertyMap.ContainsKey(uri)) then propertyMap.Add(uri, new HashSet<IRDFResource>()) 
//           propertyMap.Item(uri).Add(value)

        member this.GetPropertyByURI(uri) = this._GetPropertyByURI uri

        member this.ReplaceValuesForProperty(uri, values:IEnumerable<string>) = this._ReplaceValuesForProperty(uri, values)
        member this.ReplaceValuesForProperty(uri, values:IEnumerable<IRDFClass>) = this._ReplaceValuesForProperty(uri, values)

        member this.AddValuesToProperty(uri, values:IEnumerable<string>) = this._AddValuesToProperty(uri, values)
        member this.AddValuesToProperty(uri, values:IEnumerable<IRDFClass>) = this._AddValuesToProperty(uri, values)

        member this.RemoveValuesForProperty(uri) = this._RemoveValuesForProperty uri
        member this.RemoveObjectFromStore = this._RemoveObjectFromStore

        member this.updateObject() = this._UpdateObject()

        override this.Equals(o) = 
            match o with
            | :? IRDFClass as other -> (this:>IRDFClass).ID.ToString().Equals(other.ID.ToString())
            | _ -> false

        override this.GetHashCode() = (this:>IRDFClass).ID.ToString().GetHashCode()

        interface IRDFClass with
            member this.ID with get() = Uri(id)
 //           member this.Properties with get() = propertyMap
            member this.LiteralProperties with get() = 
                this._UpdateObject()
                literalPropertyMap
            member this.ObjectProperties with get() = 
                this._UpdateObject()
                objectPropertyMap
            //member this.Prop with get(uri) = this._GetPropertyByURI uri
            member this.Serialize = "Test"





