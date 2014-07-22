namespace LITEQ

open Sesame
open Configuration

type SesameStore(serverUri:string) = class
    let connection =
        let c = Sesame(serverUri)
        Configuration.prefixes |> List.iter( fun (k,v) ->
            c.AddPrefix(k, v)
        )
        c

    let classesFilter =
        System.Collections.Generic.HashSet<string>(
            [
            "http://www.w3.org/2002/07/owl#Ontology"; "http://www.w3.org/2002/07/owl#Class";
            "http://www.w3.org/2002/07/owl#ObjectProperty"; "http://www.w3.org/2002/07/owl#DatatypeProperty";
            "http://www.w3.org/2002/07/owl#Restriction"; "http://www.w3.org/2002/07/owl#TransitiveProperty";
            "http://www.w3.org/2002/07/owl#AnnotationProperty"; "http://www.w3.org/2002/07/owl#FunctionalProperty"; 
            "http://www.w3.org/2002/07/owl#SymmetricProperty"; "http://www.w3.org/2000/01/rdf-schema#Class"; 
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#Property"; "http://www.w3.org/2002/07/owl#InverseFunctionalProperty" ]
        )

    let usedNames = System.Collections.Generic.HashSet<string>()

    let queryForClasses = 
        let _, results = connection.Query("""
            SELECT DISTINCT ?class (SAMPLE(?c) AS ?comment) WHERE
            {
                        { ?class a rdfs:Class . } UNION { ?class a owl:Class . }
                        OPTIONAL { ?class rdfs:comment ?c . }
            }
            GROUP BY ?class
        """)
        let predefined = 
            (Map.empty<string,string>).Add("class", "http://www.w3.org/2002/07/owl#Thing").Add("label","Thing").Add("comment","Everything is a owl:Thing")
            
        (results)
        |> List.filter( fun binding ->
            not (classesFilter.Contains(binding.["class"]) || binding.["class"].StartsWith("node") || binding.["class"].StartsWith("http://www.w3.org/2000/01/rdf-schema#") )
        )
        |> List.map ( fun binding ->
            let typeName =
                if binding.ContainsKey "label" && not (binding.["label"] = "") && not(usedNames.Contains binding.["label"])
                then
                    usedNames.Add binding.["label"] |> ignore
                    binding.["label"]
                else List.fold (fun (acc:string) (abbr:string, prefix:string) -> acc.Replace(prefix,abbr+":")) binding.["class"] connection.Prefixes 
            
            let comment =
                if binding.ContainsKey "comment" && not (binding.["comment"] = "")
                then binding.["comment"]
                else "Not further documentation available"

            binding.["class"], typeName, comment
        )

    let queryForProperties = 
        let _, results = connection.Query("""
            SELECT DISTINCT ?class (SAMPLE(?c) AS ?comment) ?range ?domain WHERE
            {
                { ?class a rdf:Property . ?class rdfs:range ?range . ?class rdfs:domain ?domain . }
                UNION { ?class a owl:ObjectProperty. ?class rdfs:range ?range . ?class rdfs:domain ?domain . }
                UNION { ?class a owl:DatatypeProperty . ?class rdfs:range ?range . ?class rdfs:domain ?domain . }
                OPTIONAL { ?class rdfs:comment ?c . }
            }
            GROUP BY ?class ?range ?domain
        """)

        (results) 
        |> List.filter( fun binding ->
            not (classesFilter.Contains(binding.["class"]) || binding.["class"].StartsWith("node") || binding.["class"].StartsWith("http://www.w3.org/2000/01/rdf-schema#") )
        )
        |> List.map ( fun binding ->
            let typeName =
                if binding.ContainsKey "label" && not (binding.["label"] = "") && not(usedNames.Contains binding.["label"])
                then
                    usedNames.Add binding.["label"] |> ignore
                    binding.["label"]
                else List.fold (fun (acc:string) (abbr:string, prefix:string) -> acc.Replace(prefix,abbr+":")) binding.["class"] connection.Prefixes 
            
            let comment =
                if binding.ContainsKey "comment" && not (binding.["comment"] = "")
                then binding.["comment"]
                else "Not further documentation available"

            binding.["class"], typeName, comment, binding.["domain"], binding.["range"]
        )        

    let propertiesForType (typeUri:string) =
        let typeUri' = "<"+typeUri+">" 
        let varNames, results = connection.Query("""
            SELECT DISTINCT ?property ?comment ?range WHERE {
            {
                ?property rdfs:domain ?class .
                ?property rdfs:range ?range .
                :type: rdfs:subClassOf+ ?class.
            }
            UNION
            {
                ?property rdfs:domain :type: .
                ?property rdfs:range ?range .
            }
            UNION
            {
                ?property rdfs:domain owl:Thing .
                ?property rdfs:range ?range .
            }
            OPTIONAL { ?property rdfs:label ?label . }
            OPTIONAL { ?property rdfs:comment ?comment . }
        }
        """.Replace(":type:", typeUri'))       
        let results' = 
            if not(typeUri = "http://purl.org/ontology/mo/MusicArtist")
                then results
                else (Map.empty<string,string>
                        .Add("property","http://dbpedia.org/property/euseats")
                        .Add("range", "http://www.w3.org/2001/XMLSchema#int")) :: results
        (results)
        |> List.filter( fun binding -> not ( binding.["property"].StartsWith("http://www.w3.org/2000/01/rdf-schema#") || binding.["property"].StartsWith("http://www.w3.org/1999/02/22-rdf-syntax-ns#") ) )
        |> List.map( fun binding ->
            let propertyName = 
                if binding.ContainsKey "label" && not (binding.["label"] = "")
                then binding.["label"]
                else List.fold (fun (acc:string) (abbr:string, prefix:string) -> acc.Replace(prefix,abbr+":")) binding.["property"] connection.Prefixes 

            let comment =
                if binding.ContainsKey "comment" && not (binding.["comment"] = "")
                then binding.["comment"]
                else "Not further documentation available"

            binding.["property"], propertyName, comment, binding.["range"]
        )

    let propertyRange propertyUri = 
        let propertyUri' = "<" + propertyUri + ">"
        if propertyUri.Contains "made" then
            printfn "cool"
        let _, results = connection.Query("""
            SELECT ?range WHERE { :propertyUri: rdfs:range ?range . }
        """.Replace(":propertyUri:", propertyUri'))
        let results' = results |> List.filter( fun binding -> not(binding.["range"].StartsWith("node")) )
        if (List.length results') > 0
        then List.nth (results' |> List.map( fun binding -> binding.["range"] )) 0
        else "http://www.w3.org/2000/01/rdf-schema#Literal"

    let queryForSubclasses typeUri = 
        let typeUri' = "<" + typeUri + ">"
        let _, results = connection.Query("""
            SELECT ?class ?comment WHERE { ?class rdfs:subClassOf :type: . OPTIONAL { ?class rdfs:comment ?comment . } }
        """.Replace(":type:", typeUri'))
        results
        |> List.filter( fun binding -> not(binding.["class"] = typeUri))
        |> List.map( fun binding ->
            let comment =
                if binding.ContainsKey "comment"
                    then binding.["comment"]
                    else ""
            binding.["class"], comment )
 
    let rawQuery query = 
        let _, results = connection.Query(query)
        results

    interface IStore with
        member __.Classes = queryForClasses
        member __.Properties = queryForProperties
        member __.PropertiesForType typeUri = 
            propertiesForType(typeUri)
        member __.SubclassesForType typeUri =
            queryForSubclasses(typeUri)
        member __.RangeForProperty propertyUri = 
            propertyRange(propertyUri)
    
    member __.RawQuery query = 
        rawQuery(query)

end