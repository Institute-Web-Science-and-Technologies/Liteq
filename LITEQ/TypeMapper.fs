module TypeMapper

let private xsdMappings = 
    [
        "http://www.w3.org/2001/XMLSchema#string",              ("string", typeof<string>)    
        "http://www.w3.org/2001/XMLSchema#int",                 ("integer", typeof<int>)
        "http://www.w3.org/2001/XMLSchema#integer",             ("integer", typeof<int>)
        "http://www.w3.org/2001/XMLSchema#nonNegativeInteger",  ("integer", typeof<int>)
        "http://www.w3.org/2001/XMLSchema#decimal",             ("decimal", typeof<decimal>)
        "http://www.w3.org/2001/XMLSchema#float",               ("float", typeof<float>)
        "http://www.w3.org/2001/XMLSchema#duration",            ("string", typeof<string>)
        "http://www.w3.org/2001/XMLSchema#date",                ("string", typeof<string>)
        "http://www.w3.org/2001/XMLSchema#dateTime",            ("string", typeof<string>)
        "http://www.w3.org/2001/XMLSchema#gYear",               ("string", typeof<string>)
        "http://www.w3.org/2001/XMLSchema#gMonth",              ("string", typeof<string>)
        "http://www.w3.org/2001/XMLSchema#gYearMonth",          ("string", typeof<string>)
        "http://www.w3.org/2001/XMLSchema#gDay",                ("string", typeof<string>)
    ] |> dict

let mapPrimitiveType rangeUri = 
    if xsdMappings.ContainsKey rangeUri
    then xsdMappings.[rangeUri]
    else ("string", typeof<string>)

/// <summary>
/// Tests whether the given uri points to a primitive data type
/// </summary>
/// <param name="rangeUri">the URI to test</param>
let isPrimitive rangeUri = 
    if xsdMappings.ContainsKey rangeUri || rangeUri = "http://www.w3.org/2000/01/rdf-schema#Literal"
    then true
    else false