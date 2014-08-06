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

let queryCache : ICache<SparqlRemoteEndpoint> = createInMemoryCache()
let updateCache: ICache<SparqlRemoteUpdateEndpoint> = createInMemoryCache()

let queryCreateOrRetrieve queryUri = 
    match queryCache.TryRetrieve queryUri with
    | Some x -> x
    | None -> queryCache.Set(queryUri, SparqlRemoteEndpoint(Uri queryUri))

let updateCreateOrRetrieve updateUri = 
    match updateCache.TryRetrieve updateUri with
    | Some x -> x
    | None -> updateCache.Set(updateUri, SparqlRemoteUpdateEndpoint(Uri updateUri))


[<StructuredFormatDisplay("{InstanceUri}")>]
type RdfResourceWrapper(instanceUri, queryUri, updateUri:string option) = 
    class
        let queryConnection = queryCreateOrRetrieve queryUri
        let updateConnection = 
            if updateUri.IsSome
                then Some (updateCreateOrRetrieve updateUri.Value)
                else None
        let isUri x = System.Uri.IsWellFormedUriString(x, System.UriKind.Absolute)
        new(instanceUri, queryUri, typeUri, (updateUri:string option)) = 
            if updateUri.IsSome then
                let query = "ASK { <" + (Uri instanceUri).ToString() + "> a <" + (Uri typeUri).ToString() + "> . }"
                let con = queryCreateOrRetrieve queryUri
                if not ((con.QueryWithResultSet query).Result) then
                    (updateCreateOrRetrieve updateUri.Value).Update("INSERT DATA { <" + (Uri instanceUri).ToString() + "> a <" + (Uri typeUri).ToString() + "> . }")

            RdfResourceWrapper(instanceUri, queryUri, updateUri)
        member __.InstanceUri = instanceUri
        
        member __.Item 
            with get (propertyUri) = 
                let query = new SparqlParameterizedString("SELECT ?o WHERE { @instance @property ?o .}")
                query.SetUri("instance", Uri instanceUri)
                query.SetUri("property", Uri propertyUri)
                queryConnection.QueryWithResultSet(query.ToString())
                |> Seq.map (fun r ->
                    let v = r.["o"].ToString()
                    if v.Contains "^^"
                        then v.Substring(0, v.IndexOf "^^")
                        else v)
                |> Seq.toList 
            and set propertyUri (values : string list) =
                let enc x = 
                    if isUri x
                        then "<" + (Uri x).ToString() + ">"
                        else "\""+x+"\""
                if updateConnection.IsNone then
                    failwith "No SPARQL update endpoint specified"
                // Query for existing values since we want to replace them
                let query = "SELECT ?value WHERE { <" + (Uri instanceUri).ToString() + "> <" + (Uri propertyUri).ToString() + "> ?value .}"
                let triplePatterns = 
                    queryConnection.QueryWithResultSet(query.ToString())
                    |> Seq.map(fun r ->
                        "<" + (Uri instanceUri).ToString() + "> <" + (Uri propertyUri).ToString() + "> " + enc (r.["value"].ToString()) + " ." )
                    |> String.concat ""
                if not (triplePatterns = "") then
                    updateConnection.Value.Update("DELETE DATA { " + triplePatterns + " }")

                // Actually update the values
                let updatePattern = 
                    values 
                    |> Seq.map(fun v -> "<"+(Uri instanceUri).ToString()+"> <"+(Uri propertyUri).ToString()+"> " + enc v + " ." )
                    |> String.concat ""
                let query = "INSERT DATA { " + updatePattern + " }"
                updateConnection.Value.Update query

    end

let createInstance instanceUri queryUri (updateUri: string) = 
    if updateUri = ""
        then new RdfResourceWrapper(instanceUri, queryUri, None) :> System.Object
        else new RdfResourceWrapper(instanceUri, queryUri, Some updateUri) :> System.Object

let createInstanceWithType instanceUri queryUri typeUri (updateUri: string) = 
    if updateUri = ""
        then new RdfResourceWrapper(instanceUri, queryUri, typeUri, None) :> System.Object
        else new RdfResourceWrapper(instanceUri, queryUri, typeUri, Some updateUri) :> System.Object

let accessProperty (wrapper:RdfResourceWrapper) (propertyUri:string) = 
    wrapper.[propertyUri] :> System.Object

let setProperty (wrapper:RdfResourceWrapper) (propertyUri:string) (values:System.Object) = 
    let values' = values :?> string list
    wrapper.[propertyUri] <- values'

let QueryForInstances (u : string) (query : string) (queryUri : string) = 
    let u' = u.Replace("?","")
    let rec fetchNextOnes (offset : int) (limit : int) = 
        seq { 
            let query' = query + " LIMIT " + string (limit) + " OFFSET " + string (offset)
            let instances = 
                (queryCreateOrRetrieve queryUri).QueryWithResultSet(query') |> Seq.map (fun r -> r.[u'].ToString())
            for instanceUri in instances do
                yield RdfResourceWrapper(instanceUri, queryUri, None) :> System.Object
            if (Seq.length instances) = limit then yield! fetchNextOnes (offset + limit) limit
        }
    fetchNextOnes 0 1000

let QueryForTuples (u : string, v : string) (query : string) (queryUri : string) = 
    let u', v' = u.Replace("?",""), v.Replace("?","")
    let rec fetchNextOnes (offset : int) (limit : int) = 
        seq { 
            let query' = query + " LIMIT " + string (limit) + " OFFSET " + string (offset)
            let instances = 
                (queryCreateOrRetrieve queryUri).QueryWithResultSet(query') 
                |> Seq.map (fun r -> r.[u'].ToString(), r.[v'].ToString())
            for (u_instance, v_instance) in instances do
                yield new RdfResourceWrapper(u_instance, queryUri, None) :> System.Object, 
                      new RdfResourceWrapper(v_instance, queryUri, None) :> System.Object
            if (Seq.length instances) = limit then yield! fetchNextOnes (offset + limit) limit
        }
    fetchNextOnes 0 1000