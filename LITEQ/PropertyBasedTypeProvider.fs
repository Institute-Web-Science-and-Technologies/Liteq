namespace TypeProviderImplementation.PropertyBasedTypeProvider

open System
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Collections
open ProviderImplementation.ProvidedTypes
open System.Collections.Generic
open System.Reflection
open VDS.RDF.Query
open TypeProviderImplementation
open Schema
open Configuration
open Wrapper

type Property = string

[<TypeProvider>]
type PropertyBasedTypeProvider(config : TypeProviderConfig) as this = 
    class
        inherit TypeProviderForNamespaces()
        let ns = "Uniko.West.Liteq"
        let asm = Assembly.GetExecutingAssembly()
        let provTy = ProvidedTypeDefinition(asm, ns, "PropertyBasedRdfProvider", Some typeof<obj>)
        let propertyTypeCache = new Dictionary<string, ValueType>()
        let mutable store : IStore option = None
        let mutable conf: Configuration option = None
        let mutable isReadOnly = false

        let mutable makeLabel : string -> string = id

        
        let getPropertyType (p : Property) = 
            if propertyTypeCache.ContainsKey p then propertyTypeCache.[p]
            else 
                let v = Wrapper.ProbingQuery p conf.Value.ServerUri
                propertyTypeCache.[p] <- v
                v
        
        let convertToProperty (p : Property) = 
            let storeUri' = conf.Value.ServerUri
            let updateUri' = conf.Value.UpdateUri
            match getPropertyType p with
            | URI -> 
                let prov = 
                    ProvidedProperty
                        (propertyName = makeLabel p, 
                         propertyType = typedefof<list<_>>.MakeGenericType(typeof<RdfResourceWrapper>))
                prov.GetterCode <- fun args -> 
                    <@@ let wrapper = (%%args.[0] : RdfResourceWrapper)
                        (accessProperty wrapper p) :?> string list 
                        |> List.map (fun uri -> Wrapper.createInstance uri storeUri' updateUri' :?> RdfResourceWrapper) @@>
                if isReadOnly then 
                    prov.SetterCode <- fun args -> 
                        <@@ let wrapper = (%%args.[0] : RdfResourceWrapper)
                            let values = (%%args.[1] : RdfResourceWrapper list)
                            setProperty wrapper p (values |> List.map (fun x -> x.InstanceUri)) @@>
                prov
            | LITERAL -> 
                let prov = 
                    ProvidedProperty
                        (propertyName = makeLabel p, propertyType = typedefof<list<_>>.MakeGenericType(typeof<string>))
                prov.GetterCode <- fun args -> 
                    <@@ let wrapper = (%%args.[0] : RdfResourceWrapper)
                        (accessProperty wrapper p) :?> string list @@>
                if isReadOnly then 
                    prov.SetterCode <- fun args -> 
                        <@@ let wrapper = (%%args.[0] : RdfResourceWrapper)
                            let values = (%%args.[1] : string list)
                            setProperty wrapper p values @@>
                prov
        
        let createUnspecifcType (previouslyChosen : Property list) = 
            let storeUri' = conf.Value.ServerUri
            let updateUri' = conf.Value.UpdateUri
            let t = ProvidedTypeDefinition("Unnamed", baseType = Some typeof<RdfResourceWrapper>)
            let extensionQuery = "SELECT DISTINCT ?s WHERE { " + Transform previouslyChosen + " }" //makeSubjectQueryWithType previouslyChosen classUri
            t.AddMember
                (ProvidedProperty
                     ("Extension", typedefof<seq<_>>.MakeGenericType(t), IsStatic = true, 
                      
                      GetterCode = fun _ -> 
                          <@@ Wrapper.QueryForInstancesWithoutCasting "?s" extensionQuery storeUri' updateUri' @@>))
            previouslyChosen
            |> Seq.map convertToProperty
            |> Seq.iter t.AddMember
            if isReadOnly then 
                t.AddMember
                    (new ProvidedConstructor(parameters = [ ProvidedParameter
                                                                (parameterName = "instanceUri", 
                                                                 parameterType = typeof<string>) ], 
                                             
                                             InvokeCode = fun args -> 
                                                 <@@ createInstanceWithType (%%args.[0] : string) storeUri' 
                                                         "http://www.w3.org/2000/01/rdf-schema#Resource" updateUri' @@>))
            t
        
        let createIntersectionTypes (previouslyChosen : Property list) = 
            let storeUri' = conf.Value.ServerUri
            let updateUri' = conf.Value.UpdateUri
            let container = ProvidedTypeDefinition("Named", baseType = None)
            container.AddMembersDelayed(fun _ -> 
                Wrapper.TypesOfInstances previouslyChosen storeUri'
                |> Seq.map (fun classUri -> 
                       let t = ProvidedTypeDefinition(makeLabel classUri, baseType = Some typeof<RdfResourceWrapper>)
                       let extensionQuery = 
                           "SELECT DISTINCT ?s WHERE { ?s a <" + classUri + "> . " + Transform previouslyChosen + " }" //makeSubjectQueryWithType previouslyChosen classUri
                       t.AddMember
                           (ProvidedProperty
                                ("Extension", typedefof<seq<_>>.MakeGenericType(t), IsStatic = true, 
                                 
                                 GetterCode = fun _ -> 
                                     <@@ Wrapper.QueryForInstancesWithoutCasting "?s" extensionQuery storeUri' 
                                             updateUri' @@>))
                       Wrapper.PropertiesWith classUri conf.Value.ServerUri
                       |> Set.ofSeq
                       |> Set.union (previouslyChosen |> Set.ofSeq)
                       |> Seq.map convertToProperty
                       |> Seq.iter t.AddMember
                       if isReadOnly then 
                           t.AddMember
                               (new ProvidedConstructor(parameters = [ ProvidedParameter
                                                                           (parameterName = "instanceUri", 
                                                                            parameterType = typeof<string>) ], 
                                                        
                                                        InvokeCode = fun args -> 
                                                            <@@ createInstanceWithType (%%args.[0] : string) storeUri' 
                                                                    classUri updateUri' @@>))
                       t :> MemberInfo)
                |> Seq.toList)
            container
        
        let rec makeNestedTypes (previouslyChosen : Property list) = 
            let filter (previouslyChosen) (queryResults : string list) = 
                let previouslyChosen' = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" :: previouslyChosen
                let contains l x = l |> List.exists ((=) x)
                queryResults |> Seq.filter (fun x -> not (contains previouslyChosen' x))
            Wrapper.PropertiesOccuringWithProperties previouslyChosen conf.Value.ServerUri
            |> (filter previouslyChosen)
            |> Seq.map (fun property -> 
                   let x = ProvidedTypeDefinition(className = makeLabel property, baseType = None)
                   let updated_list = List.Cons(property, previouslyChosen)
                   x.AddMemberDelayed(fun _ -> createUnspecifcType updated_list)
                   x.AddMemberDelayed(fun _ -> createIntersectionTypes updated_list)
                   x.AddMembersDelayed(fun _ -> makeNestedTypes updated_list)
                   x :> MemberInfo)
            |> Seq.toList
        
        let buildTypes (typeName : string) (args : obj []) = 
            if conf.IsNone then 
                
                conf <- Some ( Configuration.CreateFromArgs(args) )
                isReadOnly <- String.IsNullOrWhiteSpace conf.Value.UpdateUri || 
                                not ( Uri.IsWellFormedUriString( conf.Value.UpdateUri, UriKind.Absolute ) )

                if not (System.IO.File.Exists(conf.Value.SchemaFile)) then 
                    ConversionQueries.composeGraph (new SparqlRemoteEndpoint(System.Uri conf.Value.ServerUri)) conf.Value
                store <- Some(LocalSchema(conf.Value.SchemaFile) :> IStore)
                makeLabel <- (store.Value :?> LocalSchema).makeLabel

            let t = ProvidedTypeDefinition(className = typeName, baseType = None)
            provTy.AddMember t
            t.AddMembersDelayed(fun _ -> makeNestedTypes [])
            t
 
        let parameters = [ ProvidedStaticParameter("configFile", typeof<string>, "");
                           ProvidedStaticParameter("serverUri", typeof<string>, "");
                           ProvidedStaticParameter("updateUri", typeof<string>, "");
                           ProvidedStaticParameter("schemaFile", typeof<string>, "");
                           ProvidedStaticParameter("prefixFile", typeof<string>, "")
                            ]
        do provTy.DefineStaticParameters( parameters, buildTypes )
        do this.AddNamespace(ns, [ provTy ])
    end

[<TypeProviderAssembly>]
do ()
