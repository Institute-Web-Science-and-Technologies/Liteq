module Wrapper
open Sesame


[<StructuredFormatDisplay("{InstanceUri}")>]
type RDFWrapper(instanceUri, storeName) as __ = class
    static let full_repos = System.Collections.Generic.Dictionary<string,Sesame.Sesame>()

    static let escapeValue value = 
        if System.Uri.IsWellFormedUriString(value, System.UriKind.Absolute) 
        then "<" + value + ">"
        else "\"" + value + "\""


    static member AddRepository (name:string) =
        if not (full_repos.ContainsKey name)
            then full_repos.Add (name, new Sesame.Sesame(name) )

//        if not(repositories.ContainsKey name) then
//            repositories.Add(name, (url,repo))

    static member GetRepository (name:string) = full_repos.[name] 
//        if full_repos.ContainsKey name 
//            then full_repos.[name]
//            else 
//                let url, repo = repositories.[name]
//                full_repos.Add (name, new Sesame.Sesame(url, repo))
//                full_repos.[name]

    new (instanceUri, storeName, typeUri) = 
        let instanceUri', typeUri' = "<" + instanceUri + ">", "<" + typeUri + ">"
        let query = "SELECT * WHERE { " + instanceUri' + " ?predicate " + typeUri' + " . }"
        let store = RDFWrapper.GetRepository(storeName)
        let _, results = store.Query(query)
        if results.Length = 0 then
            let insQuery = "INSERT DATA { " + instanceUri' + " rdf:type " + typeUri' + " . }" 
            store.Update(insQuery) |> ignore
            
        RDFWrapper(instanceUri, storeName)

    member __.InstanceUri =
        instanceUri

    member __.Item
        with get(propertyUri) = 
            let instanceUri', propertyUri' = "<" + instanceUri + ">", "<" + propertyUri + ">"
            let query = "SELECT ?value WHERE { " + instanceUri' + " " + propertyUri' + " ?value . }"
            let store = RDFWrapper.GetRepository(storeName)//new Sesame("http://stardog.west.uni-koblenz.de:8080/openrdf-sesame", "Jamendo")
            let _, results = store.Query(query)
            results |> List.map( fun binding -> binding.["value"] )
        and set propertyUri (values:string list) =
            let instanceUri', propertyUri' = "<" + instanceUri + ">", "<" + propertyUri + ">"
            let query = "SELECT ?value WHERE { " + instanceUri' + " " + propertyUri' + " ?value . }"
            let store = RDFWrapper.GetRepository(storeName)//new Sesame("http://stardog.west.uni-koblenz.de:8080/openrdf-sesame", "Jamendo")
            let existingValues = store.Query(query) |> snd |> List.map( fun binding -> binding.["value"] )

            if existingValues.Length > 0 then
                let patterns = existingValues |> List.map( fun value -> instanceUri' + " " + propertyUri' + " " + (escapeValue value) + " . " ) |> String.concat ""
                let delQuery = "DELETE DATA { " + patterns + "}"
                //TODO: Fix quick and dirty
                store.Update(delQuery) |> ignore

            let patterns = values |> List.map( fun value -> instanceUri' + " " + propertyUri' + " " + (escapeValue value) + " . " ) |> String.concat ""
            let insQuery = "INSERT DATA { " + patterns + "}" 
            store.Update(insQuery) |> ignore
            ()
end

let QueryForInstances (u:string) (query:string) (storeName:string) =
    let rec fetchNextOnes (offset:int) (prefetchCount:int) (u:string) (query:string) (storeName:string) = seq {
        let query' = query + " LIMIT " + string(prefetchCount) + " OFFSET " + string(offset)
        let _, results = RDFWrapper.GetRepository(storeName).Query(query')
        let instanceUris = results |> List.map( fun binding -> binding.[u] )
        for uri in instanceUris do
            yield new RDFWrapper(uri, storeName)
        if (List.length instanceUris) = prefetchCount
            then yield! fetchNextOnes (offset + prefetchCount) prefetchCount u query storeName
    }
    fetchNextOnes 0 1000 (u.Replace("?", "")) query storeName

let QueryForTuples (u:string, v:string) (query:string) (storeName:string) = 
    let rec fetchNextOnes (offset:int) (prefetchCount:int) (u:string, v:string) (query:string) (storeName:string) = seq {
        let query' = query + " LIMIT " + string(prefetchCount) + " OFFSET " + string(offset)
        let _, results = RDFWrapper.GetRepository(storeName).Query(query')
        let instanceUris = results |> List.map( fun binding -> binding.[u], binding.[v] )
        for u, v in instanceUris do
            yield new RDFWrapper(u, storeName):>System.Object, new RDFWrapper(v, storeName):>System.Object
        if (List.length instanceUris) = prefetchCount
            then yield! fetchNextOnes (offset + prefetchCount) prefetchCount (u,v) query storeName
    }
    fetchNextOnes 0 1000 (u.Replace("?", ""), v.Replace("?", "")) query storeName
