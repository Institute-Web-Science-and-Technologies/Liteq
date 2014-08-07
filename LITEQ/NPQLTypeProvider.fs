namespace LITEQ

open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Collections
open ProviderImplementation.ProvidedTypes
open System.Reflection
open VDS.RDF.Query

open Schema
open Configuration
open WrapperReimplemented

[<TypeProvider>]
type NPQLBasedTypeProvider(config : TypeProviderConfig) as this = 
    class
        inherit TypeProviderForNamespaces()

        let ns = "Uniko.West.Liteq.NPQL"
        let asm = Assembly.GetExecutingAssembly()
        let provTy = ProvidedTypeDefinition(asm, ns, "NpqlRdfProvider", Some typeof<obj>)

        let temporaryClasses = ProvidedTypeDefinition(asm, ns, "Temporary classes", None)
        let typeCache = System.Collections.Generic.Dictionary<string, ProvidedTypeDefinition>()
        let typeNames = System.Collections.Generic.Dictionary<string, string>()
        let mutable storeUri = ""
        let mutable updateUri = ""
        let mutable store = None
        
        let buildIntension (typeUri : string) (isReadOnly : bool) = 
            let store' : IStore = store.Value
            let propertiesForType = store'.PropertiesForType typeUri
            let storeUri' = storeUri
            let updateUri' = updateUri     
                   
            let properties = 
                propertiesForType
                |> List.map (fun (propertyUri, propertyName, comment, propertyRange) ->     
                                if typeCache.ContainsKey (propertyRange+"Intension")
                                    then 
                                        let t = typeCache.[propertyRange + "Intension"]
                                        let p = ProvidedProperty(propertyName, typedefof<list<_>>.MakeGenericType(t))
                                        p.GetterCode <- fun args -> 
                                                    <@@ let wrapper = (%%args.[0] : obj) :?> RdfResourceWrapper
                                                        (accessProperty wrapper propertyUri) :?> string list
                                                        |> List.map 
                                                                (fun uri -> 
                                                                WrapperReimplemented.createInstance uri storeUri' updateUri' ) @@> // new RdfResourceWrapper(uri, storeUri', None)) @@>)
                                        if not isReadOnly then
                                            p.SetterCode <- fun args ->
                                                        <@@ let wrapper = (%%args.[0] : obj) :?> RdfResourceWrapper
                                                            let value = (%%args.[1] : obj list)
                                                            value
                                                            |> List.map( fun x -> (x:?>RdfResourceWrapper).InstanceUri)
                                                            |> (setProperty wrapper propertyUri) @@>
                                        
                                        p.AddXmlDoc comment
                                        p :> MemberInfo
                                else
                                    let t, getter, setter = TypeMapper.mapPrimitive propertyUri propertyRange
                                    let p = ProvidedProperty(propertyName, typedefof<list<_>>.MakeGenericType(t))
                                    p.GetterCode <- getter
                                    if not isReadOnly then
                                        p.SetterCode <- setter              
                                    p.AddXmlDoc comment
                                    p :> MemberInfo )                
                         
            let constr = 
                new ProvidedConstructor(parameters = [ ProvidedParameter
                                                           (parameterName = "instanceUri", 
                                                            parameterType = typeof<string>) ], 
                                        InvokeCode = fun args -> 
                                            <@@ createInstanceWithType (%%args.[0] : string) storeUri' typeUri updateUri' @@>)
            (constr :> MemberInfo) :: properties
        
        let buildPropertyNavigation (typeUri : string) = 
            let store' : IStore = store.Value
            let propertiesForType = store'.PropertiesForType typeUri
            propertiesForType |> List.map (fun (propertyUri, propertyName, comment, propertyRange) -> 
                                     let t = 
                                         if not (typeCache.ContainsKey propertyUri) then 
                                             typeCache.["http://www.w3.org/2000/01/rdf-schema#Literal"]
                                         else typeCache.[propertyUri]
                                     
                                     let p = 
                                         ProvidedProperty
                                             (propertyName = propertyName, propertyType = t, 
                                              GetterCode = fun args -> 
                                                  <@@ let u, v, triples = 
                                                          (%%args.[0] : obj) :?> string * string * (string * string * string) list
                                                      let u' = u + "x"
                                                      u, v, (u, "<" + propertyUri + ">", u') :: triples @@>)
                                     
                                     p.AddXmlDoc comment
                                     p :> MemberInfo)
        
        let buildPropertyRestricitons (typeUri : string) = 
            let propertiesForType = store.Value.PropertiesForType typeUri
            propertiesForType 
            |> List.map 
                   (fun (propertyUri, propertyName, comment, propertyRange) -> 
                   ProvidedProperty(propertyName = propertyName, propertyType = typeCache.[typeUri], 
                                    GetterCode = fun args -> 
                                        <@@ let u, v, triples = 
                                                (%%args.[0] : obj) :?> string * string * (string * string * string) list
                                            let v' = v + "y"
                                            u, v', (u, "<" + propertyUri + ">", v) :: triples @@>) :> MemberInfo)
        
        let buildSubclassNavigation (typeUri : string) = 
            let subClassesForType = store.Value.SubclassesForType typeUri
            subClassesForType |> List.map (fun (subClassUri, comment) -> 
                                     let p = 
                                         ProvidedProperty
                                             (propertyName = typeNames.[subClassUri], 
                                              propertyType = typeCache.[subClassUri], 
                                              
                                              GetterCode = fun args -> 
                                                  <@@ let u, v, triples = 
                                                          (%%args.[0] : obj) :?> string * string * (string * string * string) list
                                                      let u' = u + "x"
                                                      u', v, 
                                                      (u', "rdfs:subClassOf", "<" + typeUri + ">") :: triples @@>)
                                     p.AddXmlDoc comment
                                     p)
        
        let buildTypeNavigationOptions (typeUri : string) = 
            // Build property navigation
            let propNavigationType = ProvidedTypeDefinition(typeUri + "PropNav", baseType = None)
            let propNavigationMethod = 
                ProvidedProperty
                    (propertyName = "->", propertyType = propNavigationType, 
                     GetterCode = fun args -> <@@ %%args.[0] @@>)
            temporaryClasses.AddMember propNavigationType
            propNavigationType.AddMembersDelayed(fun _ -> buildPropertyNavigation typeUri)
            // Build property restriction
            let propRestrictionType = ProvidedTypeDefinition(typeUri + "PropRes", baseType = None)
            let propRestrictionMethod = 
                ProvidedProperty
                    (propertyName = "<-", propertyType = propRestrictionType, 
                     GetterCode = fun args -> <@@ %%args.[0] @@>)
            temporaryClasses.AddMember propRestrictionType
            propRestrictionType.AddMembersDelayed(fun _ -> buildPropertyRestricitons typeUri)
            // Build subclass navigation
            let subClassNavigationType = 
                ProvidedTypeDefinition(className = typeUri + "SubclassNav", baseType = None)
            let subClassNavigationMethod = 
                ProvidedProperty
                    (propertyName = "v", propertyType = subClassNavigationType, 
                     GetterCode = fun args -> <@@ %%args.[0] @@>)
            temporaryClasses.AddMember subClassNavigationType
            subClassNavigationType.AddMembersDelayed(fun _ -> buildSubclassNavigation typeUri)
            [ subClassNavigationMethod :> MemberInfo
              propNavigationMethod :> MemberInfo
              propRestrictionMethod :> MemberInfo ]
        
        let buildPropertyNavigationOptions (propertyUri : string) = 
            let store' = store.Value
            let propertyRange = store'.RangeForProperty propertyUri
            
            let propertyRange' = 
                if not (typeCache.ContainsKey propertyRange)
                    then "http://www.w3.org/2000/01/rdf-schema#Literal"
                    else propertyRange
            
            let typeName = typeNames.[propertyRange']
            [ ProvidedProperty(propertyName = typeName, propertyType = typeCache.[propertyRange'], 
                               GetterCode = fun args -> 
                                   <@@ let u, v, triples = 
                                           (%%args.[0] : obj) :?> string * string * (string * string * string) list
                                       let u' = u + "x"
                                       u', v, (u', "a", "<" + propertyRange + ">") :: triples @@>) ]
        
        let buildTypes (typeName : string) (args : obj []) = 
            let configFilePath = args.[0] :?> string
            if store.IsNone then 
                let conf = Configuration(configFilePath)
                let x = System.IO.Path.GetFullPath(configFilePath)
                storeUri <- conf.FindConfValue KEY_SERVER_URI
                if conf.HasConfValue KEY_UPDATE_URI then
                    updateUri <- conf.FindConfValue KEY_UPDATE_URI
                if not(System.IO.File.Exists (conf.FindConfValue KEY_SCHEMA_FILE) ) then
                    ConversionQueries.composeGraph (new SparqlRemoteEndpoint(System.Uri storeUri)) conf 
                store <- Some(Schema.LocalSchema(conf.FindConfValue KEY_SCHEMA_FILE) :> IStore)

            let s = store.Value
            let isReadOnly = (updateUri = "")
            let t = ProvidedTypeDefinition(className = typeName, baseType = Some typeof<obj>)
            provTy.AddMember t
            let classes = ProvidedTypeDefinition("Classes", None)
            t.AddMember classes
            t.AddMember
                (ProvidedProperty
                     (propertyName = "IsReadOnly", propertyType = typeof<bool>, IsStatic = true, 
                      GetterCode = fun _ -> <@@ isReadOnly @@>))
            // Build types from store
            let x = 
                s.Classes |> List.map (fun (typeUri, typeName, comment) -> 
                                        let typeDef = 
                                            ProvidedTypeDefinition
                                                (className = typeName, baseType = None, HideObjectMethods = true)
                                        typeDef.AddXmlDoc comment
                                        typeDef.AddMembersDelayed(fun _ -> buildTypeNavigationOptions typeUri)
                                        let intension = 
                                            ProvidedTypeDefinition
                                                (className = "Intension", baseType = Some typeof<obj>)
                                        intension.AddMembersDelayed(fun _ -> buildIntension typeUri isReadOnly)
                                        let storeName' = storeUri
                                 
                                        let extension = 
                                            ProvidedProperty
                                                (propertyName = "Extension", 
                                                propertyType = typedefof<seq<_>>.MakeGenericType(intension), 
                                                GetterCode = fun args -> 
                                                    <@@ let u, v, triples = 
                                                            (%%args.[0] : obj) :?> string * string * (string * string * string) list
                                                  
                                                        let patternsString = 
                                                            triples
                                                            |> List.map 
                                                                    (fun (s, p, o) -> s + " " + p + " " + o + " .\n")
                                                            |> List.reduce (fun acc pattern -> acc + pattern)
                                                  
                                                        let query = 
                                                            "SELECT " + u + " WHERE {\n" + patternsString + "}"
                                                        WrapperReimplemented.QueryForInstances u query storeName' @@>)
                                        //let intension' = ProvidedProperty(propertyName="Intension'", propertyType=factoryThingy)
                                        extension.AddXmlDoc "Returns all instances that satisfy the query"
                                        typeDef.AddMembers [ extension :> MemberInfo;
                                                            intension :> MemberInfo ]
                                        if not( typeCache.ContainsKey typeUri) then
                                            typeCache.Add(typeUri, typeDef)
                                            typeCache.Add(typeUri + "Intension", intension)
                                            typeNames.Add(typeUri, typeName) 
                                        typeDef)
            classes.AddMembers(x)
            
            // Special treatment for rdfs:Literal
            let literal = ProvidedTypeDefinition(className = "Literal", baseType = None, HideObjectMethods = true)
            typeCache.Add("http://www.w3.org/2000/01/rdf-schema#Literal", literal)
            typeNames.Add("http://www.w3.org/2000/01/rdf-schema#Literal", "Literal")
            // HACK: I don't think we need these two statements
            typeCache.Add("http://www.w3.org/2001/XMLSchema#int", literal)
            typeNames.Add("http://www.w3.org/2001/XMLSchema#int", "Literal")
            classes.AddMember literal

            // Build NPQL type
            t.AddMembersDelayed(fun _ -> 
                let query = 
                    ProvidedTypeDefinition(className = "NPQL", baseType = None, HideObjectMethods = true)
                query.AddMember(ProvidedConstructor(parameters = [], 
                                                    InvokeCode = fun _ -> 
                                                        <@@ let u = "?x"
                                                            let v = "?y"
                                                            u, v, List.empty<string * string * string> @@>))
                for KeyValue(typeUri, typeName) in typeNames do
                    query.AddMember
                        (ProvidedProperty(propertyName = typeName, propertyType = typeCache.[typeUri], 
                                          GetterCode = fun args -> 
                                              <@@ let u, v, triples = 
                                                      (%%args.[0] : obj) :?> string * string * (string * string * string) list
                                                  u, v, (u, "a", "<" + typeUri + ">") :: triples @@>))
                [ query :> MemberInfo ])
            // Build properties from store
            let properties = new ProvidedTypeDefinition("Properties", None)
            t.AddMember properties
            properties.AddMembers( 
                s.Properties |> List.map (fun (propertyUri, typeName, comment, domain, range) -> 
                                    let typeDef = 
                                        ProvidedTypeDefinition
                                            (className = typeName + "Property", baseType = None, 
                                             HideObjectMethods = true)
                                    typeDef.AddXmlDoc comment
                                    typeDef.AddMembersDelayed
                                        (fun _ -> buildPropertyNavigationOptions propertyUri)
                                    if (typeCache.ContainsKey domain) && (typeCache.ContainsKey range) then 
                                        let tupleDef = 
                                            typedefof<_ * _>
                                                .MakeGenericType(typeCache.[domain], typeCache.[range])
                                        let storeName' = storeUri
                                        
                                        let extension = 
                                            ProvidedProperty
                                                (propertyName = "Extension", 
                                                 propertyType = typedefof<seq<_>>.MakeGenericType(tupleDef), //typeCache.[domain], typeCache.[range]),//propertyType=typeof<string>,
                                                                                                             
                                                 GetterCode = fun args -> 
                                                     <@@ 
                                                         let u, v, triples = 
                                                             (%%args.[0] : obj) :?> string * string * (string * string * string) list
                                                         
                                                         let patternsString = 
                                                             triples
                                                             |> List.map 
                                                                    (fun (s, p, o) -> 
                                                                    s + " " + p + " " + o + " .\n")
                                                             |> List.reduce (fun acc pattern -> acc + pattern)
                                                         
                                                         let u' = u + "x"
                                                         let query = 
                                                             "SELECT " + u + " " + u' + " WHERE {\n" 
                                                             + patternsString + "}"
                                                         WrapperReimplemented.QueryForTuples (u, u') query storeName' @@>)
                                        extension.AddXmlDoc "Returns all instances that satisfy the query"
                                        typeDef.AddMembers [ extension ]
                                    if not(typeCache.ContainsKey propertyUri) then
                                        typeCache.Add(propertyUri, typeDef)
                                        typeNames.Add(propertyUri, typeName)
                                    typeDef))
            t
        
        let parameters = 
            [ ProvidedStaticParameter("configurationFile", typeof<string>, "./liteq_default.ini") ]
        do provTy.DefineStaticParameters(parameters, buildTypes)
        do this.AddNamespace(ns, [ provTy; temporaryClasses ])  
    end

[<TypeProviderAssembly>]
do ()
