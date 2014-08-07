module Schema

open System
open VDS.RDF.Query
open VDS.RDF.Parsing
open VDS.RDF
open Configuration

// Funktioniert 👍
type IStore = 
    /// <summary>Returns a list of all classes with their label and comment</summary>
    abstract member Classes: (string*string*string) list
    /// <summary>Returns a list of all properties with their label, comment, domain and range</summary>
    abstract member Properties: (string*string*string*string*string) list
    /// <summary>Takes a type URI and returns a list containing property URI, label, comment and range</summary>
    /// <param name=typeUri>The URI of the type</param>
    abstract member PropertiesForType: string -> (string*string*string*string) list
    /// <summary>Takes a class URI and returns a list containing class URIs and labels</summary>
    /// <param name="typeUri">The URI of the type</param >
    abstract member SubclassesForType: string -> (string*string) list
    /// <summary>Takes a property URI and returns the range URI</summary>
    /// <param name="propertyUri">The URI of the property</param>
    abstract member RangeForProperty: string -> string

type LocalSchema(path : string) = 
    class
        let graph = new Graph()
        let mutable namespaces = List.empty<string*string> //Configuration.prefixes
        let niceName = NameUtils.uniqueGeneratorForUri NameUtils.niceCamelName
        let logger = new Logger.Logger(Configuration.findConfVal KEY_LOG_LEVEL |> int)

        let makeComment (r : SparqlResult) = 
            if r.HasValue("comment") && not (r.["comment"].ToString() = "") then 
                "<summary>" + r.["comment"].ToString() + "</summary>"
            else "<summary>No further documentation available</summary>"
        
        // In theory, RDFS also has a rdfs:label predicate. But 1) its more reliable to use the actual URI as a name and 2)
        // this allows you to see the namespace from which it originates
        do 
            try 
                let parser = new RdfXmlParser()
                parser.Load(graph, path)
            with 
                | _ -> 
                    failwith "Failed to parse the previously created schema file"
            namespaces <-
                graph.NamespaceMap.Prefixes
                |> Seq.map(fun prefix -> prefix, (graph.NamespaceMap.GetNamespaceUri prefix).ToString() )            
                |> Seq.toList            
        
        //TODO: Think about removing this function from this class and put it into a utils class
        member __.makeLabel (uri : string) = 
            let rec f (uri:string) ns = 
                match ns with
                | (prefix,u) :: tail ->
                    if uri.StartsWith u
                        then niceName uri (uri.Replace(u,prefix+":"))
                        else f uri tail
                | [] -> uri    
            f uri namespaces

        interface IStore with


            member __.Classes = 
                let query = "SELECT DISTINCT ?class ?comment WHERE {
                    ?class a <http://www.w3.org/2000/01/rdf-schema#Class> .
                    OPTIONAL { ?class <http://www.w3.org/2000/01/rdf-schema#comment> ?comment . }  
                }"
                (graph.ExecuteQuery(new SparqlParameterizedString(query)) :?> SparqlResultSet)
                |> Seq.map (fun r -> 
                       let uri = r.["class"].ToString()
                       let label = __.makeLabel uri
                       let comment = makeComment r
                       uri, label, comment)
                |> Seq.toList
            
            member __.Properties = 
                let query = "SELECT DISTINCT ?uri ?comment ?domain ?range WHERE {
                    ?uri <http://www.w3.org/2000/01/rdf-schema#domain> ?domain .
                    ?uri <http://www.w3.org/2000/01/rdf-schema#range> ?range .
                    OPTIONAL { ?uri <http://www.w3.org/2000/01/rdf-schema#comment> ?comment . }
                }"
                (graph.ExecuteQuery(new SparqlParameterizedString(query)) :?> SparqlResultSet)
                |> Seq.map (fun r -> 
                       let uri = r.["uri"].ToString()
                       let label = __.makeLabel uri
                       let comment = makeComment r
                       let domain = r.["domain"].ToString()
                       let range = r.["range"].ToString()
                       uri, label, comment, domain, range)
                |> Seq.toList
            
            member __.PropertiesForType typeUri = 
                let query = "SELECT DISTINCT ?uri ?comment ?range WHERE {
                    {
                        @type <http://www.w3.org/2000/01/rdf-schema#subClassOf>* ?class .
                        ?uri <http://www.w3.org/2000/01/rdf-schema#domain> ?class .
                        ?uri <http://www.w3.org/2000/01/rdf-schema#range> ?range .
                        OPTIONAL { ?uri <http://www.w3.org/2000/01/rdf-schema#comment> ?comment . }
                    } UNION {
                        ?uri <http://www.w3.org/2000/01/rdf-schema#domain> <http://www.w3.org/2002/07/owl#Thing> .
                        ?uri <http://www.w3.org/2000/01/rdf-schema#range> ?range .
                        OPTIONAL { ?uri <http://www.w3.org/2000/01/rdf-schema#comment> ?comment . }
                    }
                }"
                let parameterizedQuery = new SparqlParameterizedString(query)
                parameterizedQuery.SetUri("type", Uri typeUri)
                (graph.ExecuteQuery(parameterizedQuery) :?> SparqlResultSet)
                |> Seq.map (fun r -> 
                       let uri = r.["uri"].ToString()
                       let label = __.makeLabel uri
                       let comment = makeComment r
                       let range = r.["range"].ToString()
                       uri, label, comment, range)
                |> Seq.toList
            
            member __.SubclassesForType typeUri = 
                let query = "SELECT DISTINCT ?uri WHERE {
                    ?uri <http://www.w3.org/2000/01/rdf-schema#subClassOf> @type .
                }"
                let parameterizedQuery = new SparqlParameterizedString(query)
                parameterizedQuery.SetUri("type", Uri typeUri)
                (graph.ExecuteQuery(parameterizedQuery) :?> SparqlResultSet)
                |> Seq.map (fun r -> 
                       let uri = r.["uri"].ToString()
                       let label = __.makeLabel uri
                       uri, label)
                |> Seq.toList
            
            member __.RangeForProperty propertyUri = 
                let query = "SELECT DISTINCT ?range WHERE {
                    @property <http://www.w3.org/2000/01/rdf-schema#range> ?range .
                }"
                let parameterizedQuery = new SparqlParameterizedString(query)
                parameterizedQuery.SetUri("property", Uri propertyUri)
                (graph.ExecuteQuery(parameterizedQuery) :?> SparqlResultSet)
                |> Seq.map (fun r -> r.["range"].ToString())
                |> Seq.nth 0
    end
