(*
Copyright 2014 Stefan Scheglmann

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*)


namespace Uniko.West.LITEQTypeProvider

open System.Reflection
open System.Diagnostics
open System.IO
open System
open System.Collections.Generic
open Samples.FSharp.ProvidedTypes
open RDFTypes

type internal GenerationContext = 
    {   DomainContainer : ProvidedTypeDefinition
        QueryLimit : int
        Con : SConnector
        BaseUri: string
        StoreId: string
        CachedNavNodes : Dictionary<string, ProvidedTypeDefinition>
        CachedRDFEntities : Dictionary<string, RDFObject>
        CachedRDFInterfaces : Dictionary<string, ProvidedTypeDefinition> }
        static member create(domainContainer, queryLimit, baseUri, storeId, con) =
        {   DomainContainer = domainContainer
            QueryLimit = queryLimit
            BaseUri = baseUri
            StoreId = storeId
            Con = con
            CachedNavNodes = Dictionary<string, ProvidedTypeDefinition>()
            CachedRDFEntities = Dictionary<string, RDFObject>()
            CachedRDFInterfaces = Dictionary<string, ProvidedTypeDefinition>() }



type RDFGenerator() = 

    let useCaching:bool = true
    do Debug.WriteLineIf(Utils.cacheLoglvl.TraceInfo, "Initializing Cache with " + useCaching.ToString())

    let dictNavNodeDefs = Dictionary<string, ProvidedTypeDefinition>(HashIdentity.Structural)
    let dictTypeCache = Dictionary<string, ProvidedTypeDefinition>(HashIdentity.Structural)
//    static let dictIndCache = Dictionary<string, RDFObject>()
    let dictIndTypeCache = Dictionary<string, ProvidedTypeDefinition>()

//    static member IndCache with get() = dictIndCache 

    member internal this.generateNavNodeXMLDoc(node:NavNodeType) = 
        match node with 
            | Undefined -> ""
            | RdfClass -> ""
            | RdfIndividual -> ""
            | RdfLiteral -> ""
            | RdfProperty -> ""
            | SubTypeNav -> ""
            | SuperTypeNav -> ""
            | IndividualsNav -> ""
            | PropertyNav -> ""
            | PropertySel -> ""
            | Intension -> ""
            | Extension -> ""
            | IndividualObj -> ""
    
    member internal this.findOrCreateNavNodeDef(navName:string, navType:NavNodeType, ctx:GenerationContext) =
        let name = "NavNode_" + navName
        if useCaching then
            match dictNavNodeDefs.TryGetValue name with 
                | false,_ -> 
                    Debug.WriteLineIf(Utils.cacheLoglvl.TraceInfo, "Cache: creating " + name )
                    let t = ProvidedTypeDefinition(name, baseType=Some typeof<obj>, HideObjectMethods=true)

                    dictNavNodeDefs.Add(name, t)
                    ctx.DomainContainer.AddMember t
                    
                    (t, false)
                | _,t -> 
                    Debug.WriteLineIf(Utils.cacheLoglvl.TraceInfo, "Cache: found " + name )
                    (t, true)
        else 
            let t = ProvidedTypeDefinition(name, baseType=Some typeof<obj>, HideObjectMethods=true)
            ctx.DomainContainer.AddMember t
            (t, false)


    member internal this.findOrCreateIndividuals( individualUris, ctx:GenerationContext) = 
        [for iUri in individualUris -> this.findOrCreateIndividual( iUri, ctx)]


    member internal this.findOrCreateIndividual( iUri:string, ctx) = 
        match dictIndTypeCache.TryGetValue iUri with
            | true, ty -> iUri, ty
            | false, _ ->
                let ty = ProvidedTypeDefinition(iUri, baseType = Some typeof<RDFObject>)
                let props = ctx.Con.getPropsAndRangesFI( Uri(iUri) )
                for (prop, range) in props do
                    let rType, invokeCode = match ctx.Con.isLiteral(range) with
                    | true ->
                        typedefof<HashSet<_>>.MakeGenericType(typeof<string>),
                        fun (args:list<Quotations.Expr>) -> <@@ (%%(args.[0]) : RDFObject).GetPropertyByURI( prop ) :?> HashSet<string> @@>
                    | _ ->
                        typedefof<HashSet<_>>.MakeGenericType(typeof<IRDFClass>),
                        fun (args:list<Quotations.Expr>) -> <@@ (%%(args.[0]) : RDFObject).GetPropertyByURI( prop ) :?> HashSet<IRDFClass> @@>
            

                    let getter = ProvidedMethod(methodName = Utils.generateGetterSetterForProp(prop, "get"), 
                            parameters = [], 
                            returnType = rType, 
                            InvokeCode = invokeCode
                    )
                    getter.AddXmlDocDelayed(fun _ -> "Retrieves the value of the property '" + prop + "'" )
                    
                    let strAdd = ProvidedMethod(methodName = Utils.generateGetterSetterForProp(prop, "add"),
                        parameters = [ProvidedParameter("values", typedefof<IEnumerable<_>>.MakeGenericType(typeof<string>))],
                        returnType = typeof<unit>,
                        InvokeCode = fun args -> <@@ (%%(args.[0]) : RDFObject).AddValuesToProperty( prop, (%%(args.[1]):IEnumerable<string>) ) @@>
                    )
                    let clssAdd = ProvidedMethod(methodName = Utils.generateGetterSetterForProp(prop, "add"),
                        parameters = [ProvidedParameter("values", typedefof<IEnumerable<_>>.MakeGenericType(typeof<IRDFClass>))],
                        returnType = typeof<unit>,
                        InvokeCode = fun args -> <@@ (%%(args.[0]) : RDFObject).AddValuesToProperty( prop, (%%(args.[1]):IEnumerable<IRDFClass>) ) @@>
                    )

                    let strReplace = ProvidedMethod(methodName = Utils.generateGetterSetterForProp(prop, "replace"),
                        parameters = [ProvidedParameter("values", typedefof<IEnumerable<_>>.MakeGenericType(typeof<string>))],
                        returnType = typeof<unit>,
                        InvokeCode = fun args -> <@@ (%%(args.[0]) : RDFObject).ReplaceValuesForProperty( prop, (%%(args.[1]):IEnumerable<string>) ) @@>
                    )

                    let clssReplace = ProvidedMethod(methodName = Utils.generateGetterSetterForProp(prop, "replace"),
                        parameters = [ProvidedParameter("values", typedefof<IEnumerable<_>>.MakeGenericType(typeof<IRDFClass>))],
                        returnType = typeof<unit>,
                        InvokeCode = fun args -> <@@ (%%(args.[0]) : RDFObject).ReplaceValuesForProperty( prop, (%%(args.[1]):IEnumerable<IRDFClass>) ) @@>
                    )

                    ty.AddMembers ([getter; strAdd; clssAdd; strReplace; clssReplace])

                dictIndTypeCache.Add(iUri, ty)
                ctx.DomainContainer.AddMember ty
                iUri, ty


    member internal this.findOrCreateTypeInterface( tUri:string, ctx) = 
        match dictTypeCache.TryGetValue tUri with
            | true, ty -> ty
            | false, _ -> 
                let ty = ProvidedTypeDefinition(tUri, baseType = Some typeof<RDFObject>)
                let propsAndTypes = ctx.Con.getAllPropsAndRangesFT( Uri(tUri) )
                for (prop, range) in propsAndTypes do
                     
                    let rType, invokeCode = match ctx.Con.isLiteral(range) with
                    | true ->
                        typedefof<HashSet<_>>.MakeGenericType(typeof<string>),
                        fun (args:list<Quotations.Expr>) -> <@@ (%%(args.[0]) : RDFObject).GetPropertyByURI( prop ) :?> HashSet<string> @@>
                    | _ ->
                        typedefof<HashSet<_>>.MakeGenericType(typeof<IRDFClass>),
                        fun (args:list<Quotations.Expr>) -> <@@ (%%(args.[0]) : RDFObject).GetPropertyByURI( prop ) :?> HashSet<IRDFClass> @@>
                            
                    let getter = ProvidedMethod(methodName = Utils.generateGetterSetterForProp(prop, "get"), parameters = [], returnType = rType, InvokeCode = invokeCode) 
                    getter.AddXmlDocDelayed(fun _ -> "Retrieves the value of the property '" + prop + "'" )

                    let strAdd = ProvidedMethod(methodName = Utils.generateGetterSetterForProp(prop, "add"),
                        parameters = [ProvidedParameter("values", typedefof<IEnumerable<_>>.MakeGenericType(typeof<string>))],
                        returnType = typeof<unit>,
                        InvokeCode = fun args -> <@@ (%%(args.[0]) : RDFObject).AddValuesToProperty( prop, (%%(args.[1]):IEnumerable<string>) ) @@>
                    )
                    let clssAdd = ProvidedMethod(methodName = Utils.generateGetterSetterForProp(prop, "add"),
                        parameters = [ProvidedParameter("values", typedefof<IEnumerable<_>>.MakeGenericType(typeof<IRDFClass>))],
                        returnType = typeof<unit>,
                        InvokeCode = fun args -> <@@ (%%(args.[0]) : RDFObject).AddValuesToProperty( prop, (%%(args.[1]):IEnumerable<IRDFClass>) ) @@>
                    )

                    let strReplace = ProvidedMethod(methodName = Utils.generateGetterSetterForProp(prop, "replace"),
                        parameters = [ProvidedParameter("values", typedefof<IEnumerable<_>>.MakeGenericType(typeof<string>))],
                        returnType = typeof<unit>,
                        InvokeCode = fun args -> <@@ (%%(args.[0]) : RDFObject).ReplaceValuesForProperty( prop, (%%(args.[1]):IEnumerable<string>) ) @@>
                    )

                    let clssReplace = ProvidedMethod(methodName = Utils.generateGetterSetterForProp(prop, "replace"),
                        parameters = [ProvidedParameter("values", typedefof<IEnumerable<_>>.MakeGenericType(typeof<IRDFClass>))],
                        returnType = typeof<unit>,
                        InvokeCode = fun args -> <@@ (%%(args.[0]) : RDFObject).ReplaceValuesForProperty( prop, (%%(args.[1]):IEnumerable<IRDFClass>) ) @@>
                    )

                    ty.AddMembers ([getter; strAdd; clssAdd; strReplace; clssReplace])

                dictTypeCache.Add(tUri, ty)
                ctx.DomainContainer.AddMember ty
                ty