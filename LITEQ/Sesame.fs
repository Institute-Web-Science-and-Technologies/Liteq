module Sesame

open System.IO
open System.Net
open Microsoft.FSharp.Linq
open System.Xml
open System.Xml.Linq

type Sesame(repoUrl:string) = class
    
    //let repoUrl = "http://sparql.data.southampton.ac.uk/"
    let accept = "application/sparql-results+xml, */*;q=0.5"

    static let executeGet(url:string,accept:string) : string =
        let request = WebRequest.Create(url) :?> HttpWebRequest
        request.Accept <- accept
        request.Method <- "GET"
        
        let response = request.GetResponse()
        use reader = new StreamReader(response.GetResponseStream())
        reader.ReadToEnd()
 
    static let executePost(url:string,accept:string,data:string) : string =
        let request = WebRequest.Create(url) :?> HttpWebRequest
        request.Accept <- accept
        request.Method <- "POST"

        let postBytes= System.Text.Encoding.ASCII.GetBytes(data)
        request.ContentType <- "application/x-www-form-urlencoded";
        //request.ContentLength
        let reqStream = request.GetRequestStream()
        reqStream.Write(postBytes, 0, postBytes.Length);
        reqStream.Close()

        let response = request.GetResponse()
        use reader = new StreamReader(response.GetResponseStream())
        reader.ReadToEnd()
    
   
    static let parseResponse(response:string) = 
        let xhead = XName.Get("head", "http://www.w3.org/2005/sparql-results#")
        let xresults = XName.Get("results", "http://www.w3.org/2005/sparql-results#")
        let xvariable = XName.Get("variable", "http://www.w3.org/2005/sparql-results#")
        let xname = XName.Get("name")

        let doc = XElement.Parse(response.Trim())

        let variableNames = query {
                for v in doc.Element(xhead).Elements(xvariable) do
                select (v.Attribute(xname).Value)
        }
        let results = 
            doc.Element(xresults).Elements()
            |> Seq.map( fun r ->
                query {
                    for b in r.Elements() do
                    select (b.Attribute(xname).Value, b.Value)
                }
                |> Map.ofSeq
            )
            |> Seq.toList
        variableNames, results

    let mutable prefixes = List.empty<string*string>

    member this.AddPrefix(prefix:string, ns:string) = 
        prefixes <- (prefix,ns.Trim()) :: prefixes
        
    member this.Prefixes = 
        prefixes

    member this.Query(query:string) =
        let query' = (prefixes |> Seq.map( fun (prefix,ns) -> "PREFIX " + prefix + ":<" + ns + ">" ) |> String.concat("\n")) + "\n" + query
        let url = repoUrl + "?query=" + System.Web.HttpUtility.UrlEncode(query')
        let parsed = executeGet(url, accept) |> parseResponse
        parsed

    member this.Update(query:string) = 
        let query' = (prefixes |> Seq.map( fun (prefix,ns) -> "PREFIX " + prefix + ":<" + ns + ">" ) |> String.concat("\n")) + "\n" + query
        let url = repoUrl + "/statements?query=" + System.Web.HttpUtility.UrlEncode(query')
        
        executePost(repoUrl + "/statements", "application/sparql-update", "update="+System.Web.HttpUtility.UrlEncode(query'))

    member this.IsWritable = 
        true
end
