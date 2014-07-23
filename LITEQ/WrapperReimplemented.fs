module WrapperReimplemented

open System
open VDS.RDF.Query
open VDS.RDF.Storage
open VDS.RDF.Update
open System.Collections.Concurrent
open System.Collections.Generic

type ICache<'T> = 
    abstract TryRetrieve : string -> 'T option
    abstract Set : string * 'T -> 'T

let createInMemoryCache() = 
    let dict = new ConcurrentDictionary<_, _>()
    { new ICache<_> with
          
          member __.Set(key, value) = 
              dict.[key] <- value
              value
          
          member __.TryRetrieve(key) = 
              match dict.TryGetValue(key) with
              | true, value -> Some value
              | _ -> None }

let cache : ICache<SparqlRemoteEndpoint> = createInMemoryCache()

let createorRetrieve storeUri = 
    match cache.TryRetrieve storeUri with
    | Some x -> x
    | None -> cache.Set(storeUri, SparqlRemoteEndpoint(Uri storeUri))

[<StructuredFormatDisplay("{InstanceUri}")>]
type RdfResourceWrapper(instanceUri, storeUri) = 
    class
        let connection = createorRetrieve storeUri
        let isUri x = System.Uri.IsWellFormedUriString(x, System.UriKind.Absolute)
        new(instanceUri, storeName, typeUri) = 
            //TODO check if instance exists, if not insert into store
            RdfResourceWrapper(instanceUri, storeName)
        member __.InstanceUri = instanceUri
        
        member __.Item 
            with get (propertyUri) = 
                printfn "In get"
                let query = new SparqlParameterizedString("SELECT ?o WHERE { @instance @property ?o .}")
                query.SetUri("instance", Uri instanceUri)
                query.SetUri("property", Uri propertyUri)
                connection.QueryWithResultSet(query.ToString())
                |> Seq.map (fun r -> r.["o"].ToString())
                |> Seq.toList
            and set propertyUri (values : string list) = ()
    end

let QueryForInstances (u : string) (query : string) (storeUri : string) = 
    let u' = u.Replace("?","")
    let rec fetchNextOnes (offset : int) (limit : int) = 
        seq { 
            let query' = query + " LIMIT " + string (limit) + " OFFSET " + string (offset)
            let instances = 
                (createorRetrieve storeUri).QueryWithResultSet(query') |> Seq.map (fun r -> r.[u'].ToString())
            for instanceUri in instances do
                yield RdfResourceWrapper(instanceUri, storeUri)
            if (Seq.length instances) = limit then yield! fetchNextOnes (offset + limit) limit
        }
    fetchNextOnes 0 1000

let QueryForTuples (u : string, v : string) (query : string) (storeUri : string) = 
    let u', v' = u.Replace("?",""), v.Replace("?","")
    let rec fetchNextOnes (offset : int) (limit : int) = 
        seq { 
            let query' = query + " LIMIT " + string (limit) + " OFFSET " + string (offset)
            let instances = 
                (createorRetrieve storeUri).QueryWithResultSet(query') 
                |> Seq.map (fun r -> r.[u'].ToString(), r.[v'].ToString())
            for (u_instance, v_instance) in instances do
                yield new RdfResourceWrapper(u_instance, storeUri) :> System.Object, 
                      new RdfResourceWrapper(v_instance, storeUri) :> System.Object
            if (Seq.length instances) = limit then yield! fetchNextOnes (offset + limit) limit
        }
    fetchNextOnes 0 1000