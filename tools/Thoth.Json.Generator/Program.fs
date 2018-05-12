module CodeGenerator

open System
open System.IO
open System.Collections.Generic
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.SourceCodeServices.BasicPatterns

module Templates =
    let opening = """module rec JsonCoders

open Thot.Json
open {0}
    """

    let extension = """type {0} with
    static member Decoder =
            Decode.decode
                (fun {1} ->
                    {{ {2} }} : {0} )"""

    let required = """|> Decode.required "{0}" {1}"""

let (|NonAbbreviatedType|) (t: FSharpType) =
    let rec abbr (t: FSharpType) =
        if t.IsAbbreviation then abbr t.AbbreviatedType else t
    abbr t

let getEntityFullName (ent: FSharpEntity) =
    if ent.IsNamespace
    then match ent.Namespace with Some ns -> ns + "." + ent.CompiledName | None -> ent.CompiledName
    else defaultArg ent.TryFullName ent.CompiledName

let parse (checker: FSharpChecker) projFile =
    let projFile = System.IO.Path.GetFullPath(projFile)
    let options =
        match Path.GetExtension(projFile) with
        | ".fsx" ->
            let projCode = File.ReadAllText projFile
            checker.GetProjectOptionsFromScript(projFile, projCode)
            |> Async.RunSynchronously
            |> fst
        | ".fsproj" ->
            let opts, _, _ = Fable.CLI.ProjectCoreCracker.GetProjectOptionsFromProjectFile(projFile)
            opts
        | ext -> failwithf "Unexpected extension: %s" ext
    options
    |> checker.ParseAndCheckProject
    |> Async.RunSynchronously

type DecoderType =
    | Bool
    | String
    | Int
    | Float
    | List of DecoderType
    | Array of DecoderType
    | Object of string
    | Unkown of FSharpType

let rec fsharpTypeToDecoderType (NonAbbreviatedType t) =
    if t.HasTypeDefinition then
        match t.TypeDefinition.TryFullName with
        | Some "System.Boolean" -> Bool
        | Some "System.String" -> String
        | Some "System.Int32" -> Int
        | Some "System.Double" -> Float
        | Some "Microsoft.FSharp.Collections.FSharpList`1" ->
            List (fsharpTypeToDecoderType t.GenericArguments.[0])
        | x ->
            if t.TypeDefinition.IsArrayType then
                Array (fsharpTypeToDecoderType t.GenericArguments.[0])
            else
                Object t.TypeDefinition.CompiledName
    else
        Unkown t

let lower (s: string) =
    s |> Seq.mapi (fun i c -> match i with | 0 -> (Char.ToLower(c)) | _ -> c)  |> String.Concat

type RecordInfo =
    { FullName : string
      CompiledName : string
      StartLine : int
      StartColumn : int
      FileName : string
      Fields :  (string * DecoderType) list }

let rec printDecls decls (results : RecordInfo list)=
    match decls with
    | decl::tail ->
        match decl with
        | FSharpImplementationFileDeclaration.Entity(e, sub) ->
            if e.IsFSharpRecord then
                match e.ImplementationLocation with
                | Some location ->
                    let fields =
                        e.FSharpFields
                        |> Seq.map (fun fi -> fi.DisplayName, fsharpTypeToDecoderType fi.FieldType)
                        |> Seq.toList

                    printDecls tail ({ FullName = (getEntityFullName e)
                                       CompiledName = e.CompiledName
                                       StartLine = location.StartLine
                                       StartColumn = location.StartColumn
                                       FileName = location.FileName
                                       Fields = fields }::results)
                | None ->
                    printDecls tail results

            else
                printDecls (tail@sub) results
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(_meth, _args, _body) -> printDecls tail results
        | FSharpImplementationFileDeclaration.InitAction _expr -> printDecls tail results
    | [] -> results

let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None



[<EntryPoint>]
let main argv =
    match argv with
    | [|projFile|] ->
        try
            let checker = FSharpChecker.Create(keepAssemblyContents=true)
            let proj = parse checker projFile
            for file in proj.AssemblyContents.ImplementationFiles do
                printDecls file.Declarations []
                |> printfn "%A"

            Output.info "Generator is running"
            let mutable loop = true
            while loop do
                let input = Console.ReadLine()
                let args = input.Split(' ') |> List.ofArray
                match args with
                | "rerun" :: _->
                    let proj = parse checker projFile
                    for file in proj.AssemblyContents.ImplementationFiles do
                        printDecls file.Declarations []
                        |> printfn "%A"

                | "generate" :: filepath :: line :: col :: _ ->
                    try
                        let proj = parse checker projFile
                        for file in proj.AssemblyContents.ImplementationFiles do
                            if file.FileName = filepath then
                                let records = printDecls file.Declarations []
                                records
                                |> List.tryFind (fun record ->
                                    record.StartLine = int line && record.StartColumn = int col
                                )
                                |> function
                                | Some record -> Output.info "%A" record
                                | None -> Output.warning "Failed to find the type declaration"
                    with ex ->
                        Output.error "Cannot generate code: %s" ex.Message
                | "generate" :: _ ->
                    Output.log "Missing arguments for `generate` commands."
                    Output.log "Example usage: genenrate [absolute path file] [line] [column]"
                | ["exit"] | ["quit"] ->
                    loop <- false
                    Output.info "Closing..."
                | _ -> Output.log "Unkown command"

        with ex ->
            printfn "Cannot parse %s: %s" projFile ex.Message
    | _ ->
        Output.error "You need to pass the project file."
    0
// generate /Users/maximemangel/Workspaces/Github/MangelMaxime/Thot/tools/CodeGenerator/GeneratorTest/Main.fs 5 9
