namespace PropertyBasedTypeProvider

open System
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Collections
open ProviderImplementation.ProvidedTypes
open System.Collections.Generic
open System.Reflection
open VDS.RDF.Query

open Schema
open Configuration
open WrapperReimplemented

type Property = string

// TODO: Implement update capabilities for this provider

[<TypeProvider>]
type PropertyBasedTypeProvider(config : TypeProviderConfig) as this = 
    class
        inherit TypeProviderForNamespaces()
        let ns = "Uniko.West.Liteq.PropertyBased"
        let asm = Assembly.GetExecutingAssembly()
        let provTy = ProvidedTypeDefinition(asm, ns, "RdfProvider", Some typeof<obj>)
        let propertyTypeCache = new Dictionary<string, ValueType>()
        let mutable store : IStore option = None
        let mutable storeUri = ""
        let mutable updateUri = ""

        let mutable makeLabel : (string -> string) = id

        let getPropertyType (p : Property) = 
            if propertyTypeCache.ContainsKey p then propertyTypeCache.[p]
            else 
                let v = WrapperReimplemented.ProbingQuery p storeUri
                propertyTypeCache.[p] <- v
                v
        
        let convertToProperty (p : Property) = 
            let storeUri' = storeUri
            let updateUri' = updateUri
            match getPropertyType p with
            | URI -> 
                ProvidedProperty
                    (propertyName = makeLabel p, propertyType = typedefof<list<_>>.MakeGenericType(typeof<RdfResourceWrapper>), 
                     GetterCode = fun args -> 
                         <@@
                            let wrapper = (%%args.[0] : RdfResourceWrapper) 
                            (accessProperty wrapper p) :?> string list
                            |> List.map 
                                (fun uri -> WrapperReimplemented.createInstance uri storeUri' updateUri' :?> RdfResourceWrapper)  @@>)
            | LITERAL -> 
                ProvidedProperty
                    (propertyName = makeLabel p, propertyType = typedefof<list<_>>.MakeGenericType(typeof<string>), 
                     GetterCode = fun args -> <@@
                        let wrapper = (%%args.[0] : RdfResourceWrapper) 
                        (accessProperty wrapper p) :?> string list
                     @@>)
        
        let createUnspecifcType (previouslyChosen : Property list) = 
            let storeUri' = storeUri
            let t = ProvidedTypeDefinition("Unnamed", baseType = Some typeof<RdfResourceWrapper>)
            let extensionQuery = "SELECT DISTINCT ?s WHERE { " + Transform previouslyChosen + " }" //makeSubjectQueryWithType previouslyChosen classUri
            t.AddMember
                (ProvidedProperty
                     ("Extension", typedefof<seq<_>>.MakeGenericType(t), IsStatic = true, 
                      GetterCode = fun _ -> <@@ WrapperReimplemented.test "?s" extensionQuery storeUri' @@>))

            previouslyChosen
            |> Seq.map convertToProperty
//                match getPropertyType p with
//                    | URI -> 
//                        ProvidedProperty
//                            (propertyName = makeLabel p, propertyType = typedefof<list<_>>.MakeGenericType(typeof<RdfResourceWrapper>), 
//                             GetterCode = fun args ->  <@@
//                                                        let wrapper = (%%args.[0] : obj) :?> RdfResourceWrapper
//                                                        (accessProperty wrapper p) :?> string list
//                                                        |> List.map 
//                                                            (fun uri -> WrapperReimplemented.createInstance uri storeUri' updateUri' ) @@>) // new RdfResourceWrapper(uri, storeUri', None)) @@>)
//
//                    | LITERAL -> 
//                        ProvidedProperty
//                            (propertyName = makeLabel p, propertyType = typedefof<list<_>>.MakeGenericType(typeof<string>), 
//                             GetterCode = fun args -> <@@
//                        let wrapper = (%%args.[0] : obj) :?> RdfResourceWrapper
//                        (accessProperty wrapper p) :?> string list
//                             @@>))
            |> Seq.iter t.AddMember
            t
        
        let createIntersectionTypes (previouslyChosen : Property list) = 
            let storeUri' = storeUri
            let container = ProvidedTypeDefinition("Named", baseType = None)
            container.AddMembersDelayed(fun _ -> 
                WrapperReimplemented.TypesOfInstances previouslyChosen storeUri'
                |> Seq.map (fun (classUri) -> 
                       let t = ProvidedTypeDefinition(makeLabel classUri, baseType = Some typeof<RdfResourceWrapper>)
                       let extensionQuery = "SELECT DISTINCT ?s WHERE { ?s a <"+classUri+"> . "+Transform previouslyChosen+" }" //makeSubjectQueryWithType previouslyChosen classUri
                       t.AddMember
                           (ProvidedProperty
                                ("Extension", typedefof<seq<_>>.MakeGenericType(t), IsStatic = true, 
                                 GetterCode = fun _ -> <@@ WrapperReimplemented.test "?s" extensionQuery storeUri' @@>))          
                       WrapperReimplemented.PropertiesWith classUri storeUri
                       |> Set.ofSeq
                       |> Set.union (previouslyChosen |> Set.ofSeq)
                       |> Seq.map convertToProperty
                       |> Seq.iter t.AddMember
                       t :> MemberInfo)
                |> Seq.toList)
            container
        
        let rec makeNestedTypes (previouslyChosen : Property list) = 
            let filter (previouslyChosen) (queryResults : string list) = 
                let previouslyChosen' = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" :: previouslyChosen
                let contains l x = l |> List.exists ((=) x)
                queryResults |> Seq.filter (fun x -> not (contains previouslyChosen' x))
            
            WrapperReimplemented.PropertiesOccuringWithProperties previouslyChosen storeUri
            |> (filter previouslyChosen)
            |> Seq.map (fun property -> 
                let x = ProvidedTypeDefinition(className = makeLabel property, baseType = None)
                let updated_list = List.Cons(property, previouslyChosen)
                
                x.AddMemberDelayed(fun _ -> createUnspecifcType updated_list)
                x.AddMemberDelayed(fun _ -> createIntersectionTypes updated_list)
                x.AddMembersDelayed(fun _ -> makeNestedTypes updated_list)
                x :> MemberInfo)
            |> Seq.toList
        
        let buildTypes (typeName : string) (configFile : string) =  
                       
            if store.IsNone then
                let conf = Configuration(configFile)
                storeUri <- conf.FindConfValue KEY_SERVER_URI
                if conf.HasConfValue KEY_UPDATE_URI then
                    updateUri <- conf.FindConfValue KEY_UPDATE_URI
                if not(System.IO.File.Exists (conf.FindConfValue KEY_SCHEMA_FILE) ) then
                    ConversionQueries.composeGraph (new SparqlRemoteEndpoint(System.Uri storeUri)) conf.Prefixes (conf.FindConfValue "schemaFile") 
                store <- Some (LocalSchema(conf.FindConfValue KEY_SCHEMA_FILE) :> IStore)
                makeLabel <- (store.Value:?>LocalSchema).makeLabel

            let t = ProvidedTypeDefinition(className = typeName, baseType = None)
            provTy.AddMember t
            t.AddMembersDelayed(fun _ -> makeNestedTypes [])
            t
        
        let parameters = [ ProvidedStaticParameter("configFile", typeof<string>) ]
        do provTy.DefineStaticParameters(parameters, fun typeName args -> buildTypes typeName (args.[0] :?> string))
        do this.AddNamespace(ns, [ provTy ])
    end

[<TypeProviderAssembly>]
do ()
