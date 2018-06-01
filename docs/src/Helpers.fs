module Helpers

    open System.Collections.Generic
    open Fable.Core.JsInterop
    open Fable.Import
    open Fable.Import.Node
    open Fable.Import.Node.Globals

    let private templateCache = Dictionary<string, obj->string>()
    let private handleBarsCompile (_: string): obj->string = import "compile" "handlebars"

    let makeHtml (_:string) : string = importMember "./js/utils.js"

    /// Resolves a path to prevent using location of target JS file
    /// Note the function is inline so `__dirname` will belong to the calling file
    let inline resolve (path: string) =
        Exports.path.resolve(__dirname, path)

    /// Parses a Handlebars template
    let parseTemplate (path: string) (context: (string*obj) list) =
        let template =
            match templateCache.TryGetValue(path) with
            | true, template -> template
            | false, _ ->
                let template = Exports.fs.readFileSync(path).toString() |> handleBarsCompile
                templateCache.Add(path, template)
                template
        createObj context |> template

    /// Parses a markdown file
    let parseMarkdown (path: string) =
        Exports.fs.readFileSync(path).toString() |> makeHtml

    /// Parses a React element invoking ReactDOMServer.renderToString
    let parseReact (el: React.ReactElement) =
        ReactDomServer.renderToString el

    /// Parses a React element invoking ReactDOMServer.renderToStaticMarkup
    let parseReactStatic (el: React.ReactElement) =
        ReactDomServer.renderToStaticMarkup el

    let rec private ensureDirExists (dir: string) (cont: (unit->unit) option) =
        if Exports.fs.existsSync !^dir then
            match cont with Some c -> c() | None -> ()
        else
            ensureDirExists (Exports.path.dirname dir) (Some (fun () ->
                if not(Exports.fs.existsSync !^dir) then
                    Exports.fs?mkdirSync(dir) |> ignore
                match cont with Some c -> c() | None -> ()
            ))

    let writeFile (path: string) (content: string) =
        ensureDirExists (Exports.path.dirname path) None
        Exports.fs.writeFileSync(path, content)

    let readFile (path: string) =
        Exports.fs.readFileSync(path).toString()

    open Fable.Core
    open Fable.Helpers.React
    open Fable.Helpers.React.Props
    open Fulma

    [<Pojo>]
    type DangerousInnerHtml =
        { __html : string }

    let htmlFromMarkdown str =
        div [ DangerouslySetInnerHTML { __html = makeHtml str } ] [ ]

    let contentFromMarkdown str =
        Content.content [ Content.Props [ DangerouslySetInnerHTML { __html = makeHtml str } ] ]
            [ ]
