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

open System
open System.Text.RegularExpressions
open System.Reflection
open System.Collections.Generic
open System.IO
open Samples.FSharp.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open System.Text.RegularExpressions
open System.Diagnostics
open VDS.RDF.Query

open RDFTypes

open Microsoft.FSharp.Data



// type provider implementation
[<TypeProvider>]
type LITEQTypeProvider(config: TypeProviderConfig) as this =


    inherit TypeProviderForNamespaces()
    do Debug.WriteLineIf(Utils.lqLoglvl.TraceInfo, "Initializing LiteqTypeProvider")
    
    let monitor = obj()

    let addLine (line:string) =
        use wr = StreamWriter("d:\\dump.txt", true)
        wr.WriteLine(line)
        
    do System.Console.Beep()
    let mutable debugRekStep : int = 0
    let mutable debugRekCount : int = 0

    let getDebugSpaces(i:int) : String = 
        let mutable sp = ""
        for i=0 to i do sp <- String.Concat(sp," ")
        sp

    let ns = "Uniko.West.LITEQTypeProvider"
    let asm = Assembly.GetExecutingAssembly()
    
//    let queryCache = Dictionary<SparqlParameterizedString,SparqlResultSet>(HashIdentity.Structural)
    
    let mutable createdTypes = false

    let createTypes (rootTypeName:string) (args:obj[]) =
        Debug.WriteLineIf(Utils.lqLoglvl.TraceInfo, "creating Types")

        let schemaUrl = args.[0] :?> string
        let storeId = args.[1] :?> string
        let queryLimit = args.[2] :?> int

        let generator = new RDFGenerator()

        // create a root type
        let rootType = ProvidedTypeDefinition(asm, ns, rootTypeName, baseType=Some typeof<obj>, HideObjectMethods=true)
        ProvidedConstructor([], InvokeCode = fun _ -> <@@ new obj() @@>)
        |> rootType.AddMember

        // The basic service type
        let serviceType = ProvidedTypeDefinition("RdfService",baseType=Some typeof<obj>, HideObjectMethods=true)
        rootType.AddMember serviceType
        
        // type supposed to hide generated types 
        let domainType = ProvidedTypeDefinition("DomainTypes",baseType=Some typeof<obj>, HideObjectMethods=true)
        rootType.AddMember domainType

        // open connection to database
//        let connector = new Connector2(schemaUrl, false)

        // creating generator context + Connection to database
        let ctx = GenerationContext.create(domainType, queryLimit, schemaUrl, storeId, new SConnector(schemaUrl, storeId, true, queryLimit) )

        // entry point of the given graph
        let getBaseTypes = // ["http://www.w3.org/2000/01/rdf-schema#Resource"]
            ctx.Con.getBaseTypesForGraph()
            
        let rec createTypesRec (lastResUri:string,
                                lastResType:NavNodeType,
                                parentNavNode:ProvidedTypeDefinition, 
                                propRes:list<string>,
                                currentPropertyName:string,
                                currentUri:string,
                                currentNodeName:string,
                                currentType:NavNodeType):ProvidedTypeDefinition =  
            
            let (current, fromCache) = generator.findOrCreateNavNodeDef(currentNodeName, currentType, ctx)
            current.AddXmlDocDelayed (fun _ -> generator.generateNavNodeXMLDoc(currentType) )
            parentNavNode.AddMemberDelayed( fun _ -> ProvidedProperty( currentPropertyName, current, GetterCode = fun _ -> <@@ obj() @@>) )
            
            debugRekStep <- debugRekStep+1
            Debug.WriteLineIf(Utils.lqLoglvl.TraceInfo, getDebugSpaces(debugRekStep)+ "NODE: "+ currentNodeName ) 
            debugRekCount <- debugRekCount+1

            if (not fromCache) then
                match currentType with
                | NavNodeType.Undefined -> ()
                | NavNodeType.SubTypeNav -> 
                    match lastResType with
                        | NavNodeType.RdfClass ->
                            let subTypes = ctx.Con.getSubtypesFT( Uri(lastResUri))
                            for tUri in subTypes do
                                ignore (createTypesRec( lastResUri, lastResType, current, [], tUri, tUri, tUri, NavNodeType.RdfClass ) )
                        |_->()
                | NavNodeType.SuperTypeNav ->
                    match lastResType with
                        | NavNodeType.RdfIndividual ->
                            let superTypes = ctx.Con.getTypesFI( Uri(lastResUri))
                            for tUri in superTypes do
                                ignore (createTypesRec( lastResUri, lastResType, current, [], tUri, tUri, tUri, NavNodeType.RdfClass ) )
                        |_->()
                | NavNodeType.IndividualsNav ->
                    match lastResType with
                        | NavNodeType.RdfClass ->
                            let individuals = ctx.Con.getIndividualsFT( Uri(lastResUri), propRes)
                            for tUri in individuals do
                                ignore (createTypesRec( lastResUri, lastResType, current, [], tUri, tUri, tUri, NavNodeType.RdfIndividual ) )
                        |_->()

                | NavNodeType.PropertyNav -> 
                    // Xres->pnav(we r here)
                    // keep Xres information (lastResUri/lastResType) for further steps
                    match lastResType with
                        | NavNodeType.RdfIndividual ->
                            let indProps = ctx.Con.getPropsAndRangesFI( Uri(lastResUri))
                            for (pUri, _) in indProps do
                                ignore ( createTypesRec( lastResUri, lastResType, current, [], lastResUri + " ## " + pUri, pUri, pUri, NavNodeType.RdfProperty ) )
                        | NavNodeType.RdfClass ->
                            let classProps = ctx.Con.getAllPropsAndRangesFT( Uri(lastResUri))
                            for (pUri, _) in classProps do
                                ignore (createTypesRec( lastResUri, lastResType, current, [], pUri, pUri, pUri, NavNodeType.RdfProperty ) )
                        |_->()
            
                | NavNodeType.PropertyRes ->
                    let classProps = ctx.Con.getAllPropsAndRangesFT( Uri(lastResUri))
                    for (pUri, _) in classProps do
//                        property added to restriction list if not already present... sorted afterwards for unique naming later
                        let newPropRes = if (propRes|>List.exists ((=) pUri))then(propRes)else(pUri::propRes)|>List.sort
                        ignore (createTypesRec( lastResUri, lastResType, current, newPropRes, pUri, lastResUri , lastResUri+"_pres_"+(newPropRes|>String.concat("##")), NavNodeType.RdfClass ) )

                | NavNodeType.RdfClass ->     
                    ignore ( createTypesRec(currentUri, currentType, current, [], "PropNavigation","", currentNodeName + NavNodeType.PropertyNav.toString, NavNodeType.PropertyNav ) )
                    ignore ( createTypesRec(currentUri, currentType, current, propRes, "PropRestriction","", currentNodeName + NavNodeType.PropertyRes.toString, NavNodeType.PropertyRes ) )
                    ignore ( createTypesRec(currentUri, currentType, current, [], "SubTypeNavigation","", currentNodeName + NavNodeType.SubTypeNav.toString, NavNodeType.SubTypeNav ) )
                    ignore ( createTypesRec(currentUri, currentType, current, propRes, "Individuals","", currentNodeName + NavNodeType.IndividualsNav.toString+(propRes|>String.concat("##")), NavNodeType.IndividualsNav ) )
                    //get the 'real' RDF type content
                    //Intension -> return a class-subtype according to the current RdfClass
                    // no navnode here... return typeof<X>
//                    ignore ( createTypesRec(currentName, currentType, current, "Intension", currentName + NavNodeType.Intension.toString, NavNodeType.Intension ) )
                   
                    current.AddMemberDelayed(fun _ -> ProvidedProperty("Intension", typeof<Type> , GetterCode = fun _ -> <@@ typeof<RDFObject> @@>))
                    
                    //Extension -> return the individual objects of the current RdfClass
                    let typeInterface = generator.findOrCreateTypeInterface(currentUri, ctx)
                    let indUris = ctx.Con.getIndividualsFT( Uri(currentUri), propRes ) |>String.concat "@@@"
                    current.AddMemberDelayed(fun _ -> 
                        let baseUri = ctx.BaseUri
                        let storeId = ctx.StoreId
                        ProvidedProperty("Extension",typedefof<list<_>>.MakeGenericType(typeInterface), 
                            GetterCode = fun _ -> <@@ Regex.Split( indUris, "@@@" ) |> Seq.map (fun x -> RDFObject.create(x, baseUri, storeId):?> RDFObject ) |> Seq.toList @@>))
                    
                    
                | NavNodeType.RdfIndividual ->
                    ignore ( createTypesRec(currentUri, currentType, current, [], "PropNavigation","", currentUri + NavNodeType.PropertyNav.toString, NavNodeType.PropertyNav ) )
                    ignore ( createTypesRec(currentUri, currentType, current, [], "Types","", currentUri + NavNodeType.SuperTypeNav.toString, NavNodeType.SuperTypeNav ) )
                    let id, indObj = generator.findOrCreateIndividual( currentUri, ctx )
                    current.AddMemberDelayed(fun _ ->
                         let baseUri = ctx.BaseUri
                         let storeId = ctx.StoreId
                         ProvidedProperty("getRdfObject", indObj, GetterCode = fun _ -> <@@ RDFObject.create(currentUri, baseUri, storeId) @@>))
      
                | NavNodeType.RdfLiteral -> ()

                | NavNodeType.RdfProperty -> 
                    //Xres -> pnav -> Pres(we r here) -> Xres
                    //next type is lastResType (Xres again)
                
                    match lastResType with    
                    | NavNodeType.RdfIndividual ->
                        
                        let isLiteral, resultUris = ctx.Con.getValuesOfPropFI( Uri(lastResUri), Uri(currentUri) )
                         
                        for resUri in resultUris do
                            ignore ( createTypesRec("", currentType, current, [], resUri, resUri, resUri, 
                                                                    if (isLiteral) then NavNodeType.RdfLiteral else NavNodeType.RdfIndividual) )
                    | NavNodeType.RdfClass ->
                        let resultUris = ctx.Con.getRangeFTAP( Uri(lastResUri) , Uri(currentUri) ) 
                        for isLiteral, resUri in resultUris do
                            ignore ( createTypesRec("", currentType, current, [], resUri, resUri, resUri, NavNodeType.RdfClass ) )
                    | _->()

                | NavNodeType.IndividualObj -> ()

                | NavNodeType.Extension -> ()
//                    let individualUris = ctx.Con.getIndividualsFT( Uri(lastResUri) )
//                    let indProvTypes = generator.findOrCreateIndividuals( individualUris, ctx )
//
//                    Debug.WriteLineIf(Utils.lqLoglvl.TraceInfo, "num Individuals: " + (string)indProvTypes.Length)
//                   
//                    for (id,ty) in indProvTypes do
//                        current.AddMemberDelayed(fun _ ->
//                            let baseUri = ctx.BaseUri
//                            let storeId = ctx.StoreId
//                            ProvidedProperty( id, ty , GetterCode = fun _ -> <@@ RDFObject.create( id, baseUri, storeId) @@> ) ) //
                    
                    
            debugRekStep <- debugRekStep-1
            current
            
        addLine(System.DateTime.Now.ToLongTimeString())
        debugRekCount <- 0
        
        
        getBaseTypes|> Seq.iter( 
            fun tUri -> ignore (createTypesRec( "", NavNodeType.RdfClass, serviceType, [], tUri, tUri, tUri, NavNodeType.RdfClass) ) )


        addLine("Rekursionen: " + debugRekCount.ToString())

        let meth = ProvidedMethod("GetDataContext", [], 
                               serviceType, IsStaticMethod = true,
                               InvokeCode = (fun _ -> <@@ obj() @@>))
        meth.AddXmlDoc "<summary>Returns an instance of the RDF provider using the static paramters</summary>"
        rootType.AddMember meth

        
        rootType



     // static parameter (API to compile-time) 
    let parameters = 
        [ ProvidedStaticParameter("SchemaUrl", typeof<string>)
          ProvidedStaticParameter("StoreId", typeof<string>)
          ProvidedStaticParameter("queryLimit", typeof<int>, parameterDefaultValue = 1000) ]

    let paramRdfType = ProvidedTypeDefinition(asm, ns, "RdfNavigation", Some(typeof<obj>), HideObjectMethods = true)
     
    let helpText = "<summary>Some description of the RDF type provider</summary>                    
                   <param name='queryLimit'>Some description</param>"                 
    
    do paramRdfType.AddXmlDoc helpText  
    do paramRdfType.DefineStaticParameters(parameters, createTypes)
                  
    do this.AddNamespace(ns, [paramRdfType])

    
[<TypeProviderAssembly>]
do ()




