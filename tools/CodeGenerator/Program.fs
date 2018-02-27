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

open Thot.Json.Decode
open {0}
    """

    let extension = """type {0} with
    static member Decoder =
            decode (fun {1} -> {{ {2} }})"""

    let required = """                |> required "{0}" {1}"""

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

let getDecoderName (NonAbbreviatedType t) =
    if t.HasTypeDefinition then
        match t.TypeDefinition.TryFullName with
        | Some "System.Boolean" -> "bool"
        | Some "System.String" -> "string"
        | Some "System.Int32" -> "int"
        | Some "System.Double" -> "float"
        | _ -> t.TypeDefinition.DisplayName + ".Decoder"
    else "unknown"

let rec printDecls ns extend decls =
    for decl in decls do
        match decl with
        | FSharpImplementationFileDeclaration.Entity(e, sub) ->
            if extend then
                let fields =
                    e.FSharpFields
                    |> Seq.map (fun fi -> fi.DisplayName, getDecoderName fi.FieldType)
                    |> Seq.toList
                Console.WriteLine(Templates.extension,
                                    e.DisplayName,
                                    fields |> List.mapi (fun i _ -> "f" + string i) |> String.concat " ",
                                    fields |> List.mapi (fun i (n,_) -> sprintf "%s = f%i" n i) |> String.concat "; ")
                for (name, decoder) in fields do
                    Console.WriteLine(Templates.required, name, decoder)
                Console.WriteLine()
            else
                printDecls ns (getEntityFullName e = ns) sub
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(_meth, _args, _body) -> ()
        | FSharpImplementationFileDeclaration.InitAction _expr -> ()

[<EntryPoint>]
let main argv =
    match argv with
    | [|projFile; ns|] ->
        try
            let checker = FSharpChecker.Create(keepAssemblyContents=true)
            let proj = parse checker projFile
            Console.WriteLine(Templates.opening, ns)
            for file in proj.AssemblyContents.ImplementationFiles do
                printDecls ns false file.Declarations
        with ex ->
            printfn "Cannot parse %s: %s" projFile ex.Message
    | _ ->
        printfn "Pass the project file and the namespace containing the types as arguments."
    0
