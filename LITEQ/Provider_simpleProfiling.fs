namespace LITEQ

open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Collections
open ProviderImplementation.ProvidedTypes
open System.Reflection
open SomeName

open Wrapper
open Configuration

[<TypeProvider>]
type TypeProvider(config: TypeProviderConfig) as this = class
    inherit TypeProviderForNamespaces()

    let stopwatch = new System.Diagnostics.Stopwatch()
    do stopwatch.Start()

    let duration f = 
        let timer = new System.Diagnostics.Stopwatch()
        timer.Start()
        let returnValue = f()
        printfn "Elapsed Time: %i" timer.ElapsedMilliseconds
        returnValue

    let timestamp(msg:string) = 
        System.Diagnostics.Debug.WriteLine( stopwatch.Elapsed.Duration().TotalMilliseconds.ToString() + " - " + msg )


    let mutable conf:((string*string) list option) = None

    let ns = "Samples.Liteq"
    let asm = Assembly.GetExecutingAssembly()
    let provTy = ProvidedTypeDefinition(asm, ns, "RDFStore", Some typeof<obj>)
    let easterEgg = ProvidedTypeDefinition(asm, ns, "Temporary classes", None)
    //TODO: nach buildTypes verschieben (storeabhängig)

    let typeCache = System.Collections.Generic.Dictionary<string, ProvidedTypeDefinition>()
    let typeNames = System.Collections.Generic.Dictionary<string, string>()
    let mutable storeName = ""
    let mutable store = None

    let buildIntension (typeUri:string) (isReadOnly:bool) = 
        timestamp("buildIntension>> start - " + typeUri)
        let store':IStore = store.Value
        let propertiesForType = store'.PropertiesForType typeUri

        let properties = 
            propertiesForType
            |> List.map( fun (propertyUri, propertyName, comment, propertyRange) ->
                if not(TypeMapper.isPrimitive propertyRange)  && typeCache.ContainsKey propertyRange
                // Complex type
                then 
                    let storeName' = storeName
                    let p = ProvidedProperty(propertyName=propertyName, propertyType=typedefof<seq<_>>.MakeGenericType(typeCache.[propertyRange+"Intension"]), GetterCode = fun args ->
                        <@@
                            let wrapper = (%%args.[0]:obj) :?> RDFWrapper
                            wrapper.[propertyUri] |> List.map( fun uri -> new RDFWrapper(uri, storeName') )
                        @@>)
                    if not isReadOnly then
                        p.SetterCode <- fun args -> <@@
                            let wrapper = (%%args.[0]:obj) :?> RDFWrapper
                            let value = (%%args.[1]:list<RDFWrapper>)
                            wrapper.[propertyUri] <- (value |> List.map (fun x -> x.InstanceUri))                        
                        @@>
                    p.AddXmlDoc comment
                    p :> MemberInfo
                // Primitive type
                else
                    let propTypeName, propType = TypeMapper.mapPrimitiveType propertyRange
                    match propTypeName with
                    | "string" ->
                        let p = ProvidedProperty(propertyName=propertyName, propertyType=typedefof<list<_>>.MakeGenericType(propType),
                             GetterCode = fun args -> <@@
                                    let wrapper = (%%args.[0]:obj) :?> RDFWrapper
                                    wrapper.[propertyUri]
                                 @@>)
                        if not isReadOnly then
                            p.SetterCode <- fun args -> <@@
                                    let wrapper = (%%args.[0]:obj) :?> RDFWrapper
                                    let value = (%%args.[1]:list<string>)
                                    wrapper.[propertyUri] <- value
                                 @@>
                        p.AddXmlDoc comment
                        p :> MemberInfo
                    | "integer" ->
                        let p = ProvidedProperty(propertyName=propertyName, propertyType=typedefof<list<_>>.MakeGenericType(typeof<int>),
                             GetterCode = fun args -> 
                             <@@
                                let wrapper = (%%args.[0]:obj) :?> RDFWrapper
                                wrapper.[propertyUri] |> List.map ( fun value -> int(value) )
                             @@>)
                        if not isReadOnly then
                            p.SetterCode <- fun args -> <@@ 
                                let wrapper = (%%args.[0]:obj) :?> RDFWrapper
                                let value = (%%args.[1]:list<int>)
                                wrapper.[propertyUri] <- (value |> List.map (fun x -> string(x)))
                            @@>
                        p :> MemberInfo
                    | "decimal" ->
                        let p = ProvidedProperty(propertyName=propertyName, propertyType=typedefof<list<_>>.MakeGenericType(typeof<decimal>),
                             GetterCode = fun args -> 
                             <@@
                                let wrapper = (%%args.[0]:obj) :?> RDFWrapper
                                wrapper.[propertyUri] |> List.map ( fun value -> decimal(value) )
                             @@>)
                        if not isReadOnly then
                            p.SetterCode <- fun args -> <@@ 
                                let wrapper = (%%args.[0]:obj) :?> RDFWrapper
                                let value = (%%args.[1]:list<decimal>)
                                wrapper.[propertyUri] <- (value |> List.map (fun x -> string(x)))
                            @@>
                        p :> MemberInfo
                    | "float" ->
                        let p = ProvidedProperty(propertyName=propertyName, propertyType=typedefof<list<_>>.MakeGenericType(typeof<float>),
                             GetterCode = fun args -> 
                             <@@
                                let wrapper = (%%args.[0]:obj) :?> RDFWrapper
                                wrapper.[propertyUri] |> List.map ( fun value -> float(value) )
                             @@>)
                        if not isReadOnly then
                            p.SetterCode <- fun args -> <@@ 
                                let wrapper = (%%args.[0]:obj) :?> RDFWrapper
                                let value = (%%args.[1]:list<float>)
                                wrapper.[propertyUri] <- (value |> List.map (fun x -> string(x)))
                            @@>
                        p :> MemberInfo
                    | _ -> failwith "Undefined mapping"
            ) 
        let storeName' = storeName
        let constr = new ProvidedConstructor(parameters=[ProvidedParameter(parameterName="instanceUri", parameterType=typeof<string>)],
                         InvokeCode = fun args -> <@@ 
                            RDFWrapper.AddRepository storeName'
                            new RDFWrapper((%%args.[0]:string), storeName', typeUri) 
                         @@>)

        timestamp("buildIntension>> finish - " + typeUri)

        (constr :> MemberInfo) :: properties

    let buildPropertyNavigation (typeUri:string) = 
        timestamp("buildPropNav>> start - " + typeUri)
        let store':IStore = store.Value
        let propertiesForType = store'.PropertiesForType typeUri

        let result = propertiesForType |> List.map( fun (propertyUri, propertyName, comment, propertyRange) ->
            let p = ProvidedProperty(propertyName=propertyName, propertyType=typeCache.[propertyUri],
                GetterCode = fun args ->
                <@@
                    let u, v, triples = (%%args.[0]:obj) :?> string*string*(string*string*string) list
                    let u' = u + "x"
                    u, v, (u, "<"+propertyUri+">", u') :: triples
                @@>)
            p.AddXmlDoc comment
            p :> MemberInfo
        )
        timestamp("buildPropNav>> finish - " + typeUri)
        result

    let buildPropertyRestricitons (typeUri:string) = 
        timestamp("buildPropRes>> start - " + typeUri)
        let store':IStore = store.Value
        let propertiesForType = store'.PropertiesForType typeUri

        let result = propertiesForType |> List.map( fun (propertyUri, propertyName, comment, propertyRange) ->
            ProvidedProperty(propertyName=propertyName, propertyType=typeCache.[typeUri], 
                GetterCode = fun args ->
                <@@
                    let u, v, triples = (%%args.[0]:obj) :?> string*string*(string*string*string) list
                    let v' = v + "y"
                    u, v', (u, "<"+propertyUri+">", v) :: triples
                @@>) :> MemberInfo
        )
        timestamp("buildPropRes>> finish - " + typeUri)
        result


    let buildSubclassNavigation (typeUri:string) = 
        timestamp("subClassNav>> start - " + typeUri)
        let store':IStore = store.Value
        let subClassesForType = store'.SubclassesForType typeUri

        let result = subClassesForType |> List.map( fun (subClassUri, comment) ->
            let p = ProvidedProperty(propertyName=typeNames.[subClassUri], propertyType=typeCache.[subClassUri],
                GetterCode = fun args ->
                <@@
                    let u, v, triples = (%%args.[0]:obj) :?> string*string*(string*string*string) list
                    let u' = u + "x"
                    u', v, (u', "rdfs:subClassOf", "<"+typeUri+">") :: triples
                @@>)
            p.AddXmlDoc comment
            p
        )
        timestamp("subClassNav>> finish - " + typeUri)
        result

    let buildTypeNavigationOptions (typeUri:string) =
        timestamp("tyNavOpt>> start - " + typeUri)
        // Build property navigation
        let propNavigationType      = ProvidedTypeDefinition(typeUri+"PropNav", baseType=None)
        let propNavigationMethod    = ProvidedProperty(propertyName="ᐅ", propertyType=propNavigationType,
                                         GetterCode = fun args -> <@@ %%args.[0] @@>)
        
        easterEgg.AddMember propNavigationType
        propNavigationType.AddMembersDelayed ( fun _ -> buildPropertyNavigation typeUri )

        // Build property restriction
        let propRestrictionType     = ProvidedTypeDefinition(typeUri+"PropRes", baseType=None)
        let propRestrictionMethod   = ProvidedProperty(propertyName="ᐊ", propertyType=propRestrictionType,
                                         GetterCode = fun args -> <@@ %%args.[0] @@>) 
       
        easterEgg.AddMember propRestrictionType
        propRestrictionType.AddMembersDelayed ( fun _ -> buildPropertyRestricitons typeUri )

        // Build subclass navigation
        let subClassNavigationType  = ProvidedTypeDefinition(className=typeUri+"SubclassNav", baseType=None)
        let subClassNavigationMethod = ProvidedProperty(propertyName="ᐁ", propertyType=subClassNavigationType,
                                         GetterCode = fun args -> <@@ %%args.[0] @@>)
        
        easterEgg.AddMember subClassNavigationType
        subClassNavigationType.AddMembersDelayed ( fun _ -> buildSubclassNavigation typeUri ) 

        timestamp("tyNavOpt>> finish - " + typeUri)
        [subClassNavigationMethod :> MemberInfo ; propNavigationMethod :> MemberInfo ; propRestrictionMethod :> MemberInfo]

    let buildPropertyNavigationOptions (propertyUri:string) = 
        timestamp("PropNavOpt>> start - " + propertyUri)
        let store' = store.Value
        let propertyRange = store'.RangeForProperty propertyUri
        let propertyRange' = 
            if TypeMapper.isPrimitive propertyRange || not(typeCache.ContainsKey propertyRange)
            then "http://www.w3.org/2000/01/rdf-schema#Literal"
            else propertyRange
        let typeName = typeNames.[propertyRange']

        let result = [ ProvidedProperty(propertyName=typeName, propertyType=typeCache.[propertyRange'],
             GetterCode = fun args ->
                <@@
                    let u, v, triples = (%%args.[0]:obj) :?> string*string*(string*string*string) list
                    let u' = u + "x"
                    u', v, (u', "rdf:type", "<"+propertyRange+">") :: triples
                @@>) ]

        timestamp("PropNavOpt>> finish - " + propertyUri)
        result

    let buildTypes(typeName:string)(args:obj[]) = 
        let configFilePath = args.[0] :?> string
        let mutable isReadOnly = true

        if store.IsNone then
            Configuration.initConf configFilePath
            storeName <- Configuration.findConfVal("serverUri")
            isReadOnly <- bool.Parse( Configuration.findConfVal("isReadOnly") )
            if Configuration.hasConfVal("schemaFile") 
                then store <- Some (XMLFileStore(Configuration.findConfVal("schemaFile")) :> IStore)//
                else store <- Some (new SesameStore(storeName) :> IStore)

//            RDFWrapper.AddRepository storeName (serverUri, repository)

        let s = store.Value
        let isReadOnly' = isReadOnly

        let t = ProvidedTypeDefinition(className=typeName, baseType=Some typeof<obj>)
        provTy.AddMember t

        t.AddMember (ProvidedProperty(propertyName="IsReadOnly", propertyType=typeof<bool>,
                        IsStatic = true, GetterCode = fun _ -> <@@ isReadOnly' @@>))

        timestamp("buildTypes>> preconf buildTypes completed")

        timestamp("buildTypes>> retrieve classes + create them")
        // Build types from store
        t.AddMembersDelayed( fun _ -> 
            timestamp("buildTypes>>lazy_Classes>> start")                
            
            let result = s.Classes |> List.map( fun (typeUri, typeName, comment) ->
                let typeDef = ProvidedTypeDefinition(className=typeName, baseType=None, HideObjectMethods=true)
                typeDef.AddXmlDoc comment
                
                typeDef.AddMembersDelayed( fun _ -> buildTypeNavigationOptions typeUri )
                let intension = ProvidedTypeDefinition(className="Intension", baseType=Some typeof<obj>)
                intension.AddMembersDelayed( fun _ -> buildIntension typeUri isReadOnly' )

                let storeName' = storeName
                let extension = ProvidedProperty(propertyName="Extension",
                                    propertyType=typedefof<seq<_>>.MakeGenericType(intension),
                                     GetterCode = fun args ->
                                     <@@
                                        let u, v, triples = (%%args.[0]:obj) :?> string*string*(string*string*string) list
                                        let patternsString =
                                            triples
                                            |> List.map( fun (s, p, o) -> s + " " + p + " " + o + " .\n" ) 
                                            |> List.reduce( fun acc pattern -> acc + pattern )
                                        let query = "SELECT " + u + " WHERE {\n" + patternsString + "}"
                                        Wrapper.RDFWrapper.AddRepository storeName'
                                        Wrapper.QueryForInstances u query storeName'
                                    @@>)
                //let intension' = ProvidedProperty(propertyName="Intension'", propertyType=factoryThingy)
                extension.AddXmlDoc "Returns all instances that satisfy the query"

                typeDef.AddMembers [ extension :> MemberInfo ; intension :> MemberInfo]

                typeCache.Add(typeUri, typeDef)
                typeCache.Add(typeUri+"Intension", intension)
                typeNames.Add(typeUri, typeName)
                typeDef
            )
            timestamp("buildTypes>>lazy_Classes>> finish")
            result
        )

        timestamp("buildTypes>> create predefined classes")

        // Build predefined types
        t.AddMembersDelayed( fun _ ->
            timestamp("buildTypes>>lazy predef>> start")
            let literal = ProvidedTypeDefinition(className="Literal", baseType=None, HideObjectMethods=true)


            let query = ProvidedTypeDefinition(className="NPQL", baseType=None, HideObjectMethods=true)
            query.AddMember (ProvidedConstructor(parameters=[],
                                InvokeCode = fun _ ->
                                <@@
                                    let u = "?x"
                                    let v = "?y"
                                    u, v, List.empty<string*string*string>
                                @@>))
            for KeyValue(typeUri,typeName) in typeNames do
                query.AddMember (ProvidedProperty(propertyName=typeName, propertyType=typeCache.[typeUri],
                                    GetterCode = fun args ->
                                    <@@
                                        let u, v, triples = (%%args.[0]:obj) :?> string*string*(string*string*string) list
                                        u, v, (u, "rdf:type", "<" + typeUri + ">") :: triples
                                    @@>))
            typeCache.Add("http://www.w3.org/2000/01/rdf-schema#Literal", literal)
            typeNames.Add("http://www.w3.org/2000/01/rdf-schema#Literal", "Literal")         
            timestamp("buildTypes>>lazy predef>> finish")
            [ literal :> MemberInfo; query :> MemberInfo ]
        )

        timestamp("buildTypes>> create properties")

        // Build properties from store
        t.AddMembersDelayed ( fun _ ->
            timestamp("buildTypes>>lazy_Properties>> start")
            
            let props = s.Properties
            
            timestamp("buildTypes>>lazy_Properties>> got props from store")
            let result = props |> List.map ( fun (propertyUri, typeName, comment, domain, range) ->
                let typeDef = ProvidedTypeDefinition(className=typeName+"Property", baseType=None, HideObjectMethods=true)
                typeDef.AddXmlDoc comment
                
                typeDef.AddMembersDelayed( fun _ -> buildPropertyNavigationOptions propertyUri )
                if (typeCache.ContainsKey domain) && (typeCache.ContainsKey range) then
                    let tupleDef = typedefof<_*_>.MakeGenericType(typeCache.[domain], typeCache.[range])
                    let storeName' = storeName
                    let extension = ProvidedProperty(propertyName="Extension", propertyType=typedefof<seq<_>>.MakeGenericType(tupleDef),//typeCache.[domain], typeCache.[range]),//propertyType=typeof<string>,
                                     GetterCode = fun args ->
                                     <@@
//                                        [new RDFWrapper("http://dbtune.org/jamendo/artist/1003", storeName'):>System.Object,
//                                         new RDFWrapper("http://dbtune.org/jamendo/artist/1003", storeName'):>System.Object]
                                        let u, v, triples = (%%args.[0]:obj) :?> string*string*(string*string*string) list
                                        let patternsString =
                                            triples
                                            |> List.map( fun (s, p, o) -> s + " " + p + " " + o + " .\n" ) 
                                            |> List.reduce( fun acc pattern -> acc + pattern )
                                        let u' = u+"x"
                                        let query = "SELECT " + u + " " + u' + " WHERE {\n" + patternsString + "}"
                                        Wrapper.RDFWrapper.AddRepository storeName'
                                        Wrapper.QueryForTuples (u,u') query storeName'
                                     @@>)
                    extension.AddXmlDoc "Returns all instances that satisfy the query"

                    typeDef.AddMembers [ extension ]

                typeCache.Add(propertyUri, typeDef)
                typeNames.Add(propertyUri, typeName)
                typeDef
            )
            timestamp("buildTypes>>lazy_Properties>> finish")                
            result
        )
        timestamp("buildTypes>> finished")

        
        t

    let parameters = [ ProvidedStaticParameter("configurationFile", typeof<string>, "./liteq_default.ini") ]
    do timestamp("TP>> start building types")
    do provTy.DefineStaticParameters(parameters, buildTypes)
    do this.AddNamespace(ns, [ provTy ; easterEgg ])
end

[<assembly:TypeProviderAssembly>]
do ()