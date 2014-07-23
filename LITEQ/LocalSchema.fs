module LocalSchema

open LITEQ
open System
open VDS.RDF.Query
open VDS.RDF.Parsing
open VDS.RDF

// Funktioniert 👍
type LocalSchema(path : string) = 
    class
        let graph = new Graph()
        let mutable namespaces = List.empty<string*string>

        let makeComment (r : SparqlResult) = 
            if r.HasValue("comment") && not (r.["comment"].ToString() = "") then 
                "<summary>" + r.["comment"].ToString() + "</summary>"
            else "<summary>No further documentation available</summary>"
        
        // In theory, RDFS also has a rdfs:label predicate. But 1) its more reliable to use the actual URI as a name and 2)
        // this allows you to see the namespace from which it originates
        let makeLabel (uri : string) = 
            let rec f (uri:string) ns = 
                match ns with
                | (prefix,u) :: tail ->
                    if uri.StartsWith u
                        then uri.Replace(u,prefix)
                        else f uri tail
                | [] -> uri    
            f uri namespaces
        
        do 
            let parser = new RdfXmlParser()
            parser.Load(graph, path)
            namespaces <-
                graph.NamespaceMap.Prefixes
                |> Seq.map(fun prefix -> prefix, (graph.NamespaceMap.GetNamespaceUri prefix).ToString() )            
                |> Seq.toList            

        interface IStore with
            
            member __.Classes = 
                let query = "SELECT DISTINCT ?class ?comment WHERE {
                    ?class a <http://www.w3.org/2000/01/rdf-schema#Class> .
                    OPTIONAL { ?class <http://www.w3.org/2000/01/rdf-schema#comment> ?comment . }  
                }"
                (graph.ExecuteQuery(new SparqlParameterizedString(query)) :?> SparqlResultSet)
                |> Seq.map (fun r -> 
                       let uri = r.["class"].ToString()
                       let label = makeLabel uri
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
                       let label = makeLabel uri
                       let comment = makeComment r
                       let domain = r.["domain"].ToString()
                       let range = r.["range"].ToString()
                       uri, label, comment, domain, range)
                |> Seq.toList
            
            member __.PropertiesForType typeUri = 
                let query = "SELECT DISTINCT ?uri ?comment ?range WHERE {
                    @type <http://www.w3.org/2000/01/rdf-schema#subClassOf>* ?class .
                    ?uri <http://www.w3.org/2000/01/rdf-schema#domain> ?class .
                    ?uri <http://www.w3.org/2000/01/rdf-schema#range> ?range .
                    OPTIONAL { ?uri <http://www.w3.org/2000/01/rdf-schema#comment> ?comment . }
                }"
                let parameterizedQuery = new SparqlParameterizedString(query)
                parameterizedQuery.SetUri("type", Uri typeUri)
                (graph.ExecuteQuery(parameterizedQuery) :?> SparqlResultSet)
                |> Seq.map (fun r -> 
                       let uri = r.["uri"].ToString()
                       let label = makeLabel uri
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
                       let label = makeLabel uri
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
