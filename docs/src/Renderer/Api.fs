namespace Renderer


open Docs.Helpers
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import.React
open System.Text.RegularExpressions
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fulma.Elements

[<RequireQualifiedAccess>]
module rec Api =

    type PageConfig =
        { ActivePage : Navbar.ActivePage
          Title : string option
          Body : ReactElement }

    let private templatePath = resolve "${entryDir}/templates/template.hbs"
    let private markdownPath = resolve "${entryDir}/README.md"
    let private indexPath = resolve "${entryDir}/public/index.html"

    let [<Literal>] ApiDocHeader = "///**Description**"
    let [<Literal>] ApiDocParameters = "///**Parameters**"
    let [<Literal>] ApiDocOutput = "///**Output Type**"
    let [<Literal>] ApiDocExceptions = "///**Exceptions**"

    let (|DocComment|NotDocComment|) (str : string) =
        let trimed = str.Trim()
        if trimed.StartsWith("///") then
            DocComment
        else
            NotDocComment

    let (|IsValidParameter|_|) (str : string) =
        let regex = "\/\/\/  \* `(.+)` - parameter of type `(.+)`(?: - ?){0,1}(.*)"
        let m = Regex(regex).Match(str)
        if m.Success then
            let name = m.Groups.[1].Value
            let parameter = m.Groups.[2].Value
            let description =
                if m.Groups.[3].Value = "" then
                    None
                else
                    Some m.Groups.[3].Value
            Some (name, parameter, description)
        else
            None

    let (|IsValidOutput|_|) (str : string) =
        let regex = "\/\/\/  \* `(\w+)`"
        let m = Regex(regex).Match(str)
        if m.Success then
            Some m.Groups.[1].Value
        else
            None

    type DocCommentSection =
        | UnkownSection of string
        | DescriptionSection
        | ParametersSection
        | OutputSection
        | ExceptionsSection
        | DeclarationSection

    let stripComments (line : string) =
        line
            .Replace("///", "")
            .Trim()

    type Parameter = string * string * string option

    type FunctionInfo =
        { Description : string
          Parameters : Parameter list
          Output : string
          Exceptions : string
          Declaration : string list }

        static member Empty =
            { Description = ""
              Parameters = []
              Output = ""
              Exceptions = ""
              Declaration = [] }

        static member MapFromLines (lines : string list) =
            let rec map (lines : string list) (currentSection : DocCommentSection) (result : FunctionInfo) =
                match lines with
                | head::tail ->
                    match head with
                    | DocComment ->
                        match currentSection with
                        | DescriptionSection ->
                            match head with
                            | ApiDocParameters ->
                                map tail ParametersSection result
                            | "///" -> map tail currentSection result
                            | content ->
                                let newDescription =
                                    result.Description
                                    + "\n" + (stripComments content)
                                map tail currentSection { result with Description = newDescription }
                        | ParametersSection ->
                            match head with
                            | IsValidParameter info ->
                                map tail currentSection { result with Parameters = result.Parameters @ [ info ] }
                            | "///" -> map tail currentSection result
                            | unknown ->
                                match unknown with
                                | ApiDocOutput -> map tail OutputSection result
                                | _ -> map tail (UnkownSection unknown) result
                        | OutputSection ->
                            match head with
                            | IsValidOutput typ ->
                                map tail currentSection { result with Output = typ }
                            | "///" -> map tail currentSection result
                            | unknown ->
                                match unknown with
                                | ApiDocExceptions -> map tail ExceptionsSection result
                                | _ -> map tail (UnkownSection unknown) result
                        | ExceptionsSection ->
                            if head.Trim() = "///" then
                                map tail ExceptionsSection result
                            elif head.StartsWith("///") then
                                let newExceptions =
                                    result.Exceptions
                                    + "\n" + (stripComments head)
                                map tail ExceptionsSection { result with Exceptions = newExceptions }
                            else
                                // Inject the head back so it's parsed by the DeclarationSection
                                map (head::tail) DeclarationSection result
                        | UnkownSection text ->
                            printfn "The parser encounter an unkown section: `%s`" text
                            map tail currentSection result
                        | DeclarationSection -> failwith "DeclarationSection should not be parsed as a comment"
                    | NotDocComment ->
                        if head.Contains("=") then
                            { result with Declaration = result.Declaration @ [ head ] }
                        else
                            map tail DeclarationSection { result with Declaration = result.Declaration @ [ stripComments head ] }
                | [] -> result

            map lines DescriptionSection FunctionInfo.Empty

    [<Pojo>]
    type DangerousInnerHtml =
        { __html : string }

    let highlight (_: string) (_string) : string = importMember "./utils.js"

    let htmlFromMarkdown str =
        pre [ ]
            [ code [ ClassName "language-fsharp"
                     DangerouslySetInnerHTML { __html = converter.makeHtml str } ] [] ]

    let private viewDeclaration (decls : string list ) =
        let text = decls |> String.concat "\n"

        Heading.h6 [ Heading.isSubtitle ]
            [ str text ]

    let private viewParameter (name, typ, description) =
        let description =
            match description with
            | Some d -> str d |> Some
            | None -> None

        tr [ ]
           [ td [ ] [ str name ]
             td [ ] [ str typ ]
             td [ ] [ ofOption description ] ]


    let private viewFunction (info : FunctionInfo) =
        div [ ]
            [ hr [ ]
              viewDeclaration info.Declaration
              Table.table [ ]
                [ thead [ ]
                    [ tr [ ]
                        [ th [ ] [ str "Name" ]
                          th [ ] [ str "Type" ]
                          th [ ] [ str "Description" ] ] ]
                  tbody [ ]
                    (info.Parameters |> List.map viewParameter) ] ]

    let render (filepath : string) =
        let fileContent = readFile filepath

        let lines = fileContent.Split('\n') |> Array.toList

        let rec parseLines (lines: string list) results =
            match lines with
            | head::tail ->
                if head.Trim() = ApiDocHeader then
                    parseLines tail ((FunctionInfo.MapFromLines tail) :: results)
                else
                    parseLines tail results
            | [] -> results

        parseLines lines []
        |> List.map viewFunction
        |> div [ ]
