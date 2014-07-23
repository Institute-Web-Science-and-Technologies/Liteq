module ConversionQueries

// Namespaces: 
// RDF : http://www.w3.org/1999/02/22-rdf-syntax-ns#
// RDFS : http://www.w3.org/2000/01/rdf-schema
open System
open VDS.RDF.Query
open VDS.RDF.Writing
open VDS.RDF
open VDS.RDF.Storage

type ValueType = 
    | URI of string
    | LITERAL of string
    member self.makeNode (graph : Graph) = 
        match self with
        | URI x -> graph.CreateUriNode(Uri x) :> INode
        | LITERAL x -> graph.CreateLiteralNode(x) :> INode

let internal map1Var (v : string) (r : SparqlResult) : string = r.Value(v).ToString()
let internal isNoBlankNode (v : string) = v.StartsWith("_") |> not
let internal isNoBlankNode2 ((v, w) : string * string) = isNoBlankNode (v) && isNoBlankNode (w)
let internal mapToTriple (graph : Graph) ((s, p, o) : ValueType * ValueType * ValueType) = 
    new Triple(s.makeNode graph, p.makeNode graph, o.makeNode graph)

let internal extractClassesFromStore (connection : SparqlRemoteEndpoint) (graph : Graph) = 
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
    connection.QueryWithResultSet(query) |> Seq.map (map1Var "c")
    |> Seq.filter isNoBlankNode
    |> Seq.map (fun x -> URI x, URI p, URI o)
    |> Seq.map (mapToTriple graph)

let internal extractPropertiesFromStore (connection : SparqlRemoteEndpoint) (graph : Graph) = 
    let o = "http://www.w3.org/2000/01/rdf-schema#Property"
    let p = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
    let query = "SELECT DISTINCT ?p WHERE { ?x ?p ?y .}"
    connection.QueryWithResultSet(query)
    |> Seq.map (map1Var "p")
    |> Seq.filter isNoBlankNode
    |> Seq.map (fun x -> URI x, URI p, URI o)
    |> Seq.map (mapToTriple graph)

let internal extractSubClassRelations (connection : SparqlRemoteEndpoint) (graph : Graph) = 
    let p = "http://www.w3.org/2000/01/rdf-schema#subClassOf"
    let query = "SELECT DISTINCT ?s ?o WHERE { ?s <http://www.w3.org/2000/01/rdf-schema#subClassOf> ?o . }"
    connection.QueryWithResultSet(query)
    |> Seq.map (fun res -> (map1Var "s" res, map1Var "o" res))
    |> Seq.filter isNoBlankNode2
    |> Seq.map (fun (s, o) -> URI s, URI p, URI o)
    |> Seq.map (mapToTriple graph)

let internal extractDomainRelations (connection : SparqlRemoteEndpoint) (graph : Graph) = 
    let p = "http://www.w3.org/2000/01/rdf-schema#domain"
    let query = "SELECT DISTINCT ?s ?o WHERE { ?s <http://www.w3.org/2000/01/rdf-schema#domain> ?o . }"
    connection.QueryWithResultSet(query)
    |> Seq.map (fun res -> (map1Var "s" res, map1Var "o" res))
    |> Seq.filter isNoBlankNode2
    |> Seq.map (fun (s, o) -> URI s, URI p, URI o)
    |> Seq.map (mapToTriple graph)

let internal extractRangeRelations (connection : SparqlRemoteEndpoint) (graph : Graph) = 
    let p = "http://www.w3.org/2000/01/rdf-schema#range"
    let query = "SELECT DISTINCT ?s ?o WHERE { ?s <http://www.w3.org/2000/01/rdf-schema#range> ?o . }"
    connection.QueryWithResultSet(query)
    |> Seq.map (fun res -> (map1Var "s" res, map1Var "o" res))
    |> Seq.filter isNoBlankNode2
    |> Seq.map (fun (s, o) -> URI s, URI p, URI o)
    |> Seq.map (mapToTriple graph)

let internal extractCommentsRelations (connection : SparqlRemoteEndpoint) (graph : Graph) = 
    let p = "http://www.w3.org/2000/01/rdf-schema#comment"
    let query = "SELECT DISTINCT ?s ?o WHERE { ?s <http://www.w3.org/2000/01/rdf-schema#comment> ?o . }"
    connection.QueryWithResultSet(query)
    |> Seq.map (fun res -> (map1Var "s" res, map1Var "o" res))
    |> Seq.filter isNoBlankNode2
    |> Seq.map (fun (s, o) -> URI s, URI p, LITERAL o)
    |> Seq.map (mapToTriple graph)

let internal handleSpecialCases (graph : Graph) = ()

// Funktioniert 👍
let internal composeGraph (connection : SparqlRemoteEndpoint) (path : string) = 
    let graph = new Graph()
    [ extractClassesFromStore; extractPropertiesFromStore; extractSubClassRelations; extractDomainRelations; 
      extractRangeRelations; extractCommentsRelations ] 
    |> Seq.iter (fun f -> f connection graph |> Seq.iter (fun t -> (graph.Assert t) |> ignore))
    handleSpecialCases graph
    (new RdfXmlWriter()).Save(graph, path)

//let connection =
    //SesameHttpProtocolConnector("http://stardog.west.uni-koblenz.de:8080/openrdf-sesame","Jamendo")
    //new SparqlRemoteEndpoint(new Uri("http://stardog.west.uni-koblenz.de:8080/openrdf-sesame/repositories/Jamendo"))

//let path = @"C:\Users\Martin\Documents\test.rdf"