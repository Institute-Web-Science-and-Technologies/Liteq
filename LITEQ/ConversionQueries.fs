module TypeProviderImplementation.ConversionQueries

// Namespaces: 
// RDF : http://www.w3.org/1999/02/22-rdf-syntax-ns#
// RDFS : http://www.w3.org/2000/01/rdf-schema
open System
open VDS.RDF
open VDS.Common
open VDS.RDF.Query
open VDS.RDF.Writing
open VDS.RDF.Storage
open System.IO
open VDS.RDF.Query.Datasets
open VDS.RDF.Parsing
open TypeProviderImplementation.Configuration

type ValueType = 
    | URI of string
    | LITERAL of string
    member self.makeNode (graph : Graph) = 
        match self with
        | URI x -> graph.CreateUriNode(Uri x) :> INode
        | LITERAL x -> graph.CreateLiteralNode(x) :> INode

let internal logger = new Logger.Logger(Configuration.DEFAULT_LOG_LEVEL)
let internal map1Var (v : string) (r : SparqlResult) : string = r.Value(v).ToString()
let internal isNoBlankNode (v : string) = v.StartsWith("_") |> not
let internal isNoBlankNode2 ((v, w) : string * string) = isNoBlankNode (v) && isNoBlankNode (w)
let internal mapToTriple (graph : Graph) ((s, p, o) : ValueType * ValueType * ValueType) = 
    new Triple(s.makeNode graph, p.makeNode graph, o.makeNode graph)

let internal extractClassesFromStore (connection : SparqlRemoteEndpoint) (prefixGraph:IGraph) (graph : Graph) = 
    let query = "SELECT DISTINCT ?c WHERE
        {
            {?c a <http://www.w3.org/2000/01/rdf-schema#Class> .}
            UNION { ?x a ?c . }
            UNION { ?c <http://www.w3.org/2000/01/rdf-schema#subClassOf> ?y . }
            UNION { ?z <http://www.w3.org/2000/01/rdf-schema#subClassOf> ?c . }
            UNION { ?w <http://www.w3.org/2000/01/rdf-schema#domain> ?c . }
        }"
    let o = "http://www.w3.org/2000/01/rdf-schema#Class"
    let p = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
    let r = (prefixGraph.ExecuteQuery(query) :?> SparqlResultSet)
    r.Results.AddRange(connection.QueryWithResultSet(query).Results)
    r
    |> Seq.map (map1Var "c")
    |> Seq.filter isNoBlankNode
    |> Seq.map (fun x -> URI x, URI p, URI o)
    |> Seq.map (mapToTriple graph)

let internal extractPropertiesFromStore (connection : SparqlRemoteEndpoint) (prefixGraph:IGraph) (graph : Graph) = 
    let o = "http://www.w3.org/2000/01/rdf-schema#Property"
    let p = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
    let query = "SELECT DISTINCT ?p WHERE { ?x ?p ?y .}"
    let r = (prefixGraph.ExecuteQuery(query) :?> SparqlResultSet)
    r.Results.AddRange(connection.QueryWithResultSet(query).Results)
    r
    |> Seq.map (map1Var "p")
    |> Seq.filter isNoBlankNode
    |> Seq.map (fun x -> URI x, URI p, URI o)
    |> Seq.map (mapToTriple graph)

let internal extractSubClassRelations (connection : SparqlRemoteEndpoint) (prefixGraph:IGraph) (graph : Graph) = 
    let p = "http://www.w3.org/2000/01/rdf-schema#subClassOf"
    let query = "SELECT DISTINCT ?s ?o WHERE { ?s <http://www.w3.org/2000/01/rdf-schema#subClassOf> ?o . }"
    let r = (prefixGraph.ExecuteQuery(query) :?> SparqlResultSet)
    r.Results.AddRange(connection.QueryWithResultSet(query).Results)
    r
    |> Seq.map (fun res -> (map1Var "s" res, map1Var "o" res))
    |> Seq.filter isNoBlankNode2
    |> Seq.map (fun (s, o) -> URI s, URI p, URI o)
    |> Seq.map (mapToTriple graph)

let internal extractDomainRelations (connection : SparqlRemoteEndpoint) (prefixGraph:IGraph) (graph : Graph) = 
    let p = "http://www.w3.org/2000/01/rdf-schema#domain"
    let query = "SELECT DISTINCT ?s ?o WHERE { ?s <http://www.w3.org/2000/01/rdf-schema#domain> ?o . }"
    let r = (prefixGraph.ExecuteQuery(query) :?> SparqlResultSet)
    r.Results.AddRange(connection.QueryWithResultSet(query).Results)
    r
    |> Seq.map (fun res -> (map1Var "s" res, map1Var "o" res))
    |> Seq.filter isNoBlankNode2
    |> Seq.map (fun (s, o) -> URI s, URI p, URI o)
    |> Seq.map (mapToTriple graph)

let internal extractRangeRelations (connection : SparqlRemoteEndpoint) (prefixGraph:IGraph) (graph : Graph) = 
    let p = "http://www.w3.org/2000/01/rdf-schema#range"
    let query = "SELECT DISTINCT ?s ?o WHERE {
        ?s <http://www.w3.org/2000/01/rdf-schema#range> ?o .
    }"
    let r = (prefixGraph.ExecuteQuery(query) :?> SparqlResultSet)
    r.Results.AddRange(connection.QueryWithResultSet(query).Results)
    r
    |> Seq.map (fun res -> (map1Var "s" res, map1Var "o" res))
    |> Seq.filter isNoBlankNode2
    |> Seq.map (fun (s, o) -> URI s, URI p, URI o)
    |> Seq.map (mapToTriple graph)

let internal extractCommentsRelations (connection : SparqlRemoteEndpoint) (prefixGraph:IGraph) (graph : Graph) = 
    let p = "http://www.w3.org/2000/01/rdf-schema#comment"
    let query = "SELECT DISTINCT ?s ?o WHERE {
        ?s <http://www.w3.org/2000/01/rdf-schema#comment> ?o .
        FILTER(LANG(?o) = \"\" || LANGMATCHES(LANG(?o), \"en\"))
    }"
    let r = (prefixGraph.ExecuteQuery(query) :?> SparqlResultSet)
    r.Results.AddRange(connection.QueryWithResultSet(query).Results)
    r
    |> Seq.map (fun res -> (map1Var "s" res, map1Var "o" res))
    |> Seq.filter isNoBlankNode2
    |> Seq.map (fun (s, o) -> URI s, URI p, LITERAL o)
    |> Seq.map (mapToTriple graph)

// Currently no special cases defined (let's hope it stays that way)
let internal handleSpecialCases (graph : Graph) = ()

let internal handlePrefixes (definedPrefixes:Uri list) = 
    let add (f:TripleStore) (u:Uri) =
        try
            f.AddFromUri(u,true) |> ignore
        with 
            | _ -> logger.Warn ("Exception when accessing"+ u.ToString())

    let store = new TripleStore()
    definedPrefixes |> Seq.iter (add store)

    store.Graphs
    |> Seq.fold(fun (acc:IGraph) (g:IGraph) ->
        acc.Merge g
        acc) (new Graph() :> IGraph)

// Works 👍 
let internal composeGraph (connection : SparqlRemoteEndpoint) (conf:Configuration) =
    let path = conf.SchemaFile 
    let prefixUris = conf.Prefixes  
    let graph = new Graph()

    // TODO: Data stored in prefixes should also be downloaded (by dereferencing the prefix uri)
    let prefixes = new Graph() //handlePrefixes userDefinedPrefixes
    [ extractClassesFromStore; extractPropertiesFromStore; extractSubClassRelations; extractDomainRelations; 
        extractRangeRelations(*; extractCommentsRelations*) ] 
    |> Seq.iter (fun f -> f connection prefixes graph |> Seq.iter (fun t -> (graph.Assert t) |> ignore))
    handleSpecialCases graph
    
    prefixUris
    |> Seq.map(fun (x,y) -> x, Uri y)
    |> Seq.iter graph.NamespaceMap.AddNamespace
    
    let x = (new RdfXmlWriter())
    x.Save(graph, path)