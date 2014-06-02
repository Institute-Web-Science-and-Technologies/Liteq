module XmlGeneration

open System.Xml
open System.Xml.Linq


type XmlGenerator(file:string) = 
    let out = new XmlTextWriter(file, System.Text.UTF8Encoding())

    member this.WriteElem(name:string, attribs:seq<string*string>, ?enc:(unit->unit) list) = 
        out.WriteStartElement(name)
        attribs |> Seq.iter (fun attrib -> out.WriteAttributeString(fst(attrib), snd(attrib)))
        if enc.IsSome then
            enc.Value |> List.iter (fun f -> f())
        out.WriteStartElement("/"+name)
        out.WriteString("\n")
        out.Flush()

    member this.WriteClass( (uri,_,comment):string*_*string, superclasses:string list) =
        this.WriteElem("owl:Class", [ ("rdf:about", uri )], 
            [ fun () -> this.WriteElem( "rdfs:comment", [("xml:lang","en")],
                            [fun ()-> out.WriteString(comment)]) ] |> List.append (      
                                      superclasses|> List.map (fun sc -> (fun () -> this.WriteElem( "rdfs:subClassOf", [("rdf:resource", sc)]) ))
                                      )
        )
    //binding.["class"], typeName, comment, binding.["domain"], binding.["range"]
    member this.WriteProperty( (uri,_,comment,domain,range):string*_*string*string*string) =
        this.WriteElem("rdf:Property", [ ("rdf:about", uri ) ], 
                            [ fun () -> this.WriteElem( "rdfs:comment", [("xml:lang","en")], [fun ()-> out.WriteString(comment)]); 
                              fun () -> this.WriteElem("rdfs:range", [ ("rdf:resource", range) ]);
                              fun () -> this.WriteElem("rdfs:domain", [ ("rdf:resource", domain)])  ] )

    member this.Close() = out.Close()

