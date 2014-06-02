namespace LITEQ

open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Collections
open ProviderImplementation.ProvidedTypes
open System.Reflection
open SomeName
open Wrapper
open Configuration

[<TypeProvider>]
type TypeProvider(config : TypeProviderConfig) as this = 
    class
        inherit TypeProviderForNamespaces()
        
        let duration f = 
            let timer = new System.Diagnostics.Stopwatch()
            timer.Start()
            let returnValue = f()
            printfn "Elapsed Time: %i" timer.ElapsedMilliseconds
            returnValue
        
        let mutable conf : (string * string) list option = None
        let ns = "Uniko.Liteq"
        let asm = Assembly.GetExecutingAssembly()
        let provTy = ProvidedTypeDefinition(asm, ns, "RDFStore", Some typeof<obj>)
        let easterEgg = ProvidedTypeDefinition(asm, ns, "Temporary classes", None)
        //TODO: nach buildTypes verschieben (storeabhängig)
        let typeCache = System.Collections.Generic.Dictionary<string, ProvidedTypeDefinition>()
        let typeNames = System.Collections.Generic.Dictionary<string, string>()
        let mutable storeName = ""
        let mutable store = None
        
        let buildIntension (typeUri : string) (isReadOnly : bool) = 
            let store' : IStore = store.Value
            let propertiesForType = store'.PropertiesForType typeUri
            
            let properties = 
                propertiesForType |> List.map (fun (propertyUri, propertyName, comment, propertyRange) -> 
                                         if not (TypeMapper.isPrimitive propertyRange) 
                                            && typeCache.ContainsKey propertyRange // Complex type
                                                                                   then 
                                             let storeName' = storeName
                                             
                                             let p = 
                                                 ProvidedProperty
                                                     (propertyName = propertyName, 
                                                      
                                                      propertyType = typedefof<seq<_>>
                                                          .MakeGenericType(typeCache.[propertyRange 
                                                                                      + "Intension"]), 
                                                      
                                                      GetterCode = fun args -> 
                                                          <@@ let wrapper = (%%args.[0] : obj) :?> RDFWrapper
                                                              wrapper.[propertyUri] 
                                                              |> List.map 
                                                                     (fun uri -> 
                                                                     new RDFWrapper(uri, storeName')) @@>)
                                             if not isReadOnly then 
                                                 p.SetterCode <- fun args -> 
                                                     <@@ let wrapper = (%%args.[0] : obj) :?> RDFWrapper
                                                         let value = (%%args.[1] : list<RDFWrapper>)
                                                         wrapper.[propertyUri] <- (value 
                                                                                   |> List.map 
                                                                                          (fun x -> 
                                                                                          x.InstanceUri)) @@>
                                             p.AddXmlDoc comment
                                             p :> MemberInfo
                                         // Primitive type
                                         else 
                                             let propTypeName, _ = TypeMapper.mapPrimitiveType propertyRange
                                             match propTypeName with
                                             | "string" -> 
                                                 let p = 
                                                     ProvidedProperty(propertyName = propertyName, 
                                                                      
                                                                      propertyType = typedefof<list<_>>
                                                                          .MakeGenericType(typeof<string>), 
                                                                      GetterCode = fun args -> 
                                                                          <@@ let wrapper = 
                                                                                  (%%args.[0] : obj) :?> RDFWrapper
                                                                              wrapper.[propertyUri] @@>)
                                                 if not isReadOnly then 
                                                     p.SetterCode <- fun args -> 
                                                         <@@ let wrapper = (%%args.[0] : obj) :?> RDFWrapper
                                                             let value = (%%args.[1] : list<string>)
                                                             wrapper.[propertyUri] <- value @@>
                                                 p.AddXmlDoc comment
                                                 p :> MemberInfo
                                             | "integer" -> 
                                                 let p = 
                                                     ProvidedProperty
                                                         (propertyName = propertyName, 
                                                          
                                                          propertyType = typedefof<list<_>>
                                                              .MakeGenericType(typeof<int>), 
                                                          
                                                          GetterCode = fun args -> 
                                                              <@@ let wrapper = 
                                                                      (%%args.[0] : obj) :?> RDFWrapper
                                                                  wrapper.[propertyUri] 
                                                                  |> List.map (fun value -> int (value)) @@>)
                                                 if not isReadOnly then 
                                                     p.SetterCode <- fun args -> 
                                                         <@@ let wrapper = (%%args.[0] : obj) :?> RDFWrapper
                                                             let value = (%%args.[1] : list<int>)
                                                             wrapper.[propertyUri] <- (value 
                                                                                       |> List.map 
                                                                                              (fun x -> 
                                                                                              string (x))) @@>
                                                 p :> MemberInfo
                                             | "decimal" -> 
                                                 let p = 
                                                     ProvidedProperty
                                                         (propertyName = propertyName, 
                                                          
                                                          propertyType = typedefof<list<_>>
                                                              .MakeGenericType(typeof<decimal>), 
                                                          
                                                          GetterCode = fun args -> 
                                                              <@@ let wrapper = 
                                                                      (%%args.[0] : obj) :?> RDFWrapper
                                                                  wrapper.[propertyUri] 
                                                                  |> List.map (fun value -> decimal (value)) @@>)
                                                 if not isReadOnly then 
                                                     p.SetterCode <- fun args -> 
                                                         <@@ let wrapper = (%%args.[0] : obj) :?> RDFWrapper
                                                             let value = (%%args.[1] : list<decimal>)
                                                             wrapper.[propertyUri] <- (value 
                                                                                       |> List.map 
                                                                                              (fun x -> 
                                                                                              string (x))) @@>
                                                 p :> MemberInfo
                                             | "float" -> 
                                                 let p = 
                                                     ProvidedProperty
                                                         (propertyName = propertyName, 
                                                          
                                                          propertyType = typedefof<list<_>>
                                                              .MakeGenericType(typeof<float>), 
                                                          
                                                          GetterCode = fun args -> 
                                                              <@@ let wrapper = 
                                                                      (%%args.[0] : obj) :?> RDFWrapper
                                                                  wrapper.[propertyUri] 
                                                                  |> List.map (fun value -> float (value)) @@>)
                                                 if not isReadOnly then 
                                                     p.SetterCode <- fun args -> 
                                                         <@@ let wrapper = (%%args.[0] : obj) :?> RDFWrapper
                                                             let value = (%%args.[1] : list<float>)
                                                             wrapper.[propertyUri] <- (value 
                                                                                       |> List.map 
                                                                                              (fun x -> 
                                                                                              string (x))) @@>
                                                 p :> MemberInfo
                                             | _ -> failwith "Undefined mapping")
            
            let storeName' = storeName
            
            let constr = 
                new ProvidedConstructor(parameters = [ ProvidedParameter
                                                           (parameterName = "instanceUri", 
                                                            parameterType = typeof<string>) ], 
                                        InvokeCode = fun args -> 
                                            <@@ RDFWrapper.AddRepository storeName'
                                                new RDFWrapper((%%args.[0] : string), storeName', typeUri) @@>)
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
            let store' : IStore = store.Value
            let propertiesForType = store'.PropertiesForType typeUri
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
            let store' : IStore = store.Value
            let subClassesForType = store'.SubclassesForType typeUri
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
            easterEgg.AddMember propNavigationType
            propNavigationType.AddMembersDelayed(fun _ -> buildPropertyNavigation typeUri)
            // Build property restriction
            let propRestrictionType = ProvidedTypeDefinition(typeUri + "PropRes", baseType = None)
            let propRestrictionMethod = 
                ProvidedProperty
                    (propertyName = "<-", propertyType = propRestrictionType, 
                     GetterCode = fun args -> <@@ %%args.[0] @@>)
            easterEgg.AddMember propRestrictionType
            propRestrictionType.AddMembersDelayed(fun _ -> buildPropertyRestricitons typeUri)
            // Build subclass navigation
            let subClassNavigationType = 
                ProvidedTypeDefinition(className = typeUri + "SubclassNav", baseType = None)
            let subClassNavigationMethod = 
                ProvidedProperty
                    (propertyName = "v", propertyType = subClassNavigationType, 
                     GetterCode = fun args -> <@@ %%args.[0] @@>)
            easterEgg.AddMember subClassNavigationType
            subClassNavigationType.AddMembersDelayed(fun _ -> buildSubclassNavigation typeUri)
            [ subClassNavigationMethod :> MemberInfo
              propNavigationMethod :> MemberInfo
              propRestrictionMethod :> MemberInfo ]
        
        let buildPropertyNavigationOptions (propertyUri : string) = 
            let store' = store.Value
            let propertyRange = store'.RangeForProperty propertyUri
            
            let propertyRange' = 
                if TypeMapper.isPrimitive propertyRange || not (typeCache.ContainsKey propertyRange) then 
                    "http://www.w3.org/2000/01/rdf-schema#Literal"
                else propertyRange
            
            let typeName = typeNames.[propertyRange']
            [ ProvidedProperty(propertyName = typeName, propertyType = typeCache.[propertyRange'], 
                               GetterCode = fun args -> 
                                   <@@ let u, v, triples = 
                                           (%%args.[0] : obj) :?> string * string * (string * string * string) list
                                       let u' = u + "x"
                                       u', v, (u', "rdf:type", "<" + propertyRange + ">") :: triples @@>) ]
        
        let buildTypes (typeName : string) (args : obj []) = 
            let configFilePath = args.[0] :?> string
            let mutable isReadOnly = true
            if store.IsNone then 
                Configuration.initConf configFilePath
                storeName <- Configuration.findConfVal ("serverUri")
                isReadOnly <- bool.Parse(Configuration.findConfVal ("isReadOnly"))
                if Configuration.hasConfVal ("schemaFile") then 
                    store <- Some(XMLFileStore(Configuration.findConfVal ("schemaFile")) :> IStore) //
                else store <- Some(new SesameStore(storeName) :> IStore)
            //            RDFWrapper.AddRepository storeName (serverUri, repository)
            let s = store.Value
            let isReadOnly' = isReadOnly
            let t = ProvidedTypeDefinition(className = typeName, baseType = Some typeof<obj>)
            provTy.AddMember t
            t.AddMember
                (ProvidedProperty
                     (propertyName = "IsReadOnly", propertyType = typeof<bool>, IsStatic = true, 
                      GetterCode = fun _ -> <@@ isReadOnly' @@>))
            // Build types from store
            t.AddMembersDelayed(fun _ -> 
                s.Classes |> List.map (fun (typeUri, typeName, comment) -> 
                                 let typeDef = 
                                     ProvidedTypeDefinition
                                         (className = typeName, baseType = None, HideObjectMethods = true)
                                 typeDef.AddXmlDoc comment
                                 typeDef.AddMembersDelayed(fun _ -> buildTypeNavigationOptions typeUri)
                                 let intension = 
                                     ProvidedTypeDefinition
                                         (className = "Intension", baseType = Some typeof<obj>)
                                 intension.AddMembersDelayed(fun _ -> buildIntension typeUri isReadOnly')
                                 let storeName' = storeName
                                 
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
                                                  Wrapper.RDFWrapper.AddRepository storeName'
                                                  Wrapper.QueryForInstances u query storeName' @@>)
                                 //let intension' = ProvidedProperty(propertyName="Intension'", propertyType=factoryThingy)
                                 extension.AddXmlDoc "Returns all instances that satisfy the query"
                                 typeDef.AddMembers [ extension :> MemberInfo
                                                      intension :> MemberInfo ]
                                 typeCache.Add(typeUri, typeDef)
                                 typeCache.Add(typeUri + "Intension", intension)
                                 typeNames.Add(typeUri, typeName)
                                 typeDef))
            // Build predefined types
            t.AddMembersDelayed(fun _ -> 
                let literal = 
                    ProvidedTypeDefinition(className = "Literal", baseType = None, HideObjectMethods = true)
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
                                                  u, v, (u, "rdf:type", "<" + typeUri + ">") :: triples @@>))
                typeCache.Add("http://www.w3.org/2000/01/rdf-schema#Literal", literal)
                typeNames.Add("http://www.w3.org/2000/01/rdf-schema#Literal", "Literal")
                typeCache.Add("http://www.w3.org/2001/XMLSchema#int", literal)
                typeNames.Add("http://www.w3.org/2001/XMLSchema#int", "Literal")
                [ literal :> MemberInfo
                  query :> MemberInfo ])
            // Build properties from store
            t.AddMembersDelayed(fun _ -> 
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
                                        let storeName' = storeName
                                        
                                        let extension = 
                                            ProvidedProperty
                                                (propertyName = "Extension", 
                                                 propertyType = typedefof<seq<_>>.MakeGenericType(tupleDef), //typeCache.[domain], typeCache.[range]),//propertyType=typeof<string>,
                                                                                                             
                                                 GetterCode = fun args -> 
                                                     <@@ //                                        [new RDFWrapper("http://dbtune.org/jamendo/artist/1003", storeName'):>System.Object,
                                                         //                                         new RDFWrapper("http://dbtune.org/jamendo/artist/1003", storeName'):>System.Object]
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
                                                         Wrapper.RDFWrapper.AddRepository storeName'
                                                         Wrapper.QueryForTuples (u, u') query storeName' @@>)
                                        extension.AddXmlDoc "Returns all instances that satisfy the query"
                                        typeDef.AddMembers [ extension ]
                                    typeCache.Add(propertyUri, typeDef)
                                    typeNames.Add(propertyUri, typeName)
                                    typeDef))
            t
        
        let parameters = 
            [ ProvidedStaticParameter("configurationFile", typeof<string>, "./liteq_default.ini") ]
        do provTy.DefineStaticParameters(parameters, buildTypes)
        do this.AddNamespace(ns, [ provTy; easterEgg ])
    end

[<TypeProviderAssembly>]
do ()
