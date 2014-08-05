module ConversionQueries

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
open Configuration

type ValueType = 
    | URI of string
    | LITERAL of string
    member self.makeNode (graph : Graph) = 
        match self with
        | URI x -> graph.CreateUriNode(Uri x) :> INode
        | LITERAL x -> graph.CreateLiteralNode(x) :> INode

let internal logger = new Logger.Logger(Configuration.findConfVal KEY_LOG_LEVEL |> int)
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
    let query = "SELECT DISTINCT ?s ?o WHERE { ?s <http://www.w3.org/2000/01/rdf-schema#range> ?o . }"
    let r = (prefixGraph.ExecuteQuery(query) :?> SparqlResultSet)
    r.Results.AddRange(connection.QueryWithResultSet(query).Results)
    r
    |> Seq.map (fun res -> (map1Var "s" res, map1Var "o" res))
    |> Seq.filter isNoBlankNode2
    |> Seq.map (fun (s, o) -> URI s, URI p, URI o)
    |> Seq.map (mapToTriple graph)

let internal extractCommentsRelations (connection : SparqlRemoteEndpoint) (prefixGraph:IGraph) (graph : Graph) = 
    let p = "http://www.w3.org/2000/01/rdf-schema#comment"
    let query = "SELECT DISTINCT ?s ?o WHERE { ?s <http://www.w3.org/2000/01/rdf-schema#comment> ?o . }"
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
let internal composeGraph (connection : SparqlRemoteEndpoint) (prefixUris:(string*string) list) (path : string) =     
    let graph = new Graph()
    let userDefinedPrefixes = 
        if Configuration.hasConfVal KEY_GATHER_PREFIX_DATA && (bool.Parse(Configuration.findConfVal KEY_GATHER_PREFIX_DATA))
            then prefixUris |> List.map(fun (_,y) -> Uri y)
            else []
    
    let prefixes = new Graph()//handlePrefixes userDefinedPrefixes
    [ extractClassesFromStore; extractPropertiesFromStore; extractSubClassRelations; extractDomainRelations; 
        extractRangeRelations; extractCommentsRelations ] 
    |> Seq.iter (fun f -> f connection prefixes graph |> Seq.iter (fun t -> (graph.Assert t) |> ignore))
    handleSpecialCases graph
    let x = (new RdfXmlWriter())
    x.Save(graph, path)
    
//let connection =
//    new SparqlRemoteEndpoint(new Uri("http://stardog.west.uni-koblenz.de:8080/openrdf-sesame/repositories/Jamendo"))
//
//let path = @"C:\Users\Martin\Documents\test.rdf"
//
//let p = [
//    "tags","http://www.holygoat.co.uk/owl/redwood/0.1/tags/"
//    "dc","http://purl.org/dc/elements/1.1/"
//    "time","http://www.w3.org/2006/time#"
//    "tl","http://purl.org/NET/c4dm/timeline.owl#"
//    "mo","http://purl.org/ontology/mo/"
//    "foaf","http://xmlns.com/foaf/0.1/"
//    "event","http://purl.org/NET/c4dm/event.owl#"
//    "xsd","http://www.w3.org/2001/XMLSchema#"
//    "rdf","http://www.w3.org/1999/02/22-rdf-syntax-ns#"
//    "rdfs","http://www.w3.org/2000/01/rdf-schema#"
//    "dct","http://purl.org/dc/terms/"
//    "owl","http://www.w3.org/2002/07/owl#"
//    "vs","http://www.w3.org/2003/06/sw-vocab-status/ns#"
//    "skos","http://www.w3.org/2004/02/skos/core#"
//    "tzont","http://www.w3.org/2006/timezone#"
//    "daml","http://www.daml.org/2001/03/daml+oil#"
//    "dcterms","http://purl.org/dc/terms/"
//    "geo","http://www.w3.org/2003/01/geo/wgs84_pos#"
//    "frbr","http://purl.org/vocab/frbr/core#"
//    "keys","http://purl.org/NET/c4dm/keys.owl#"
//    "wot","http://xmlns.com/wot/0.1/"
//    "ao","http://purl.org/ontology/ao/core#"
//    "bio","http://purl.org/vocab/bio/0.1/"
//    "cc","http://web.resource.org/cc/" 
//]