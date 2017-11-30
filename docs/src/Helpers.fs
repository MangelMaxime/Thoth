namespace Docs

module Helpers =

    open System
    open System.Collections.Generic
    open Fable.Core.JsInterop
    open Fable.Import
    open Fable.Import.Node.Globals
    open Fable.Import.Node.Exports

    let private templateCache = Dictionary<string, obj->string>()
    let private handleBarsCompile (_: string): obj->string = import "compile" "handlebars"
    // let private marked (markdown: string): string = importDefault "marked"

    let converter = Showdown.Globals.Converter.Create()

    /// Resolves a path to prevent using location of target JS file
    /// Note the function is inline so `__dirname` will belong to the calling file
    let inline resolve (path: string) =
        Path.resolve(__dirname, path)

    /// Parses a Handlebars template
    let parseTemplate (path: string) (context: (string*obj) list) =
        let template =
            match templateCache.TryGetValue(path) with
            | true, template -> template
            | false, _ ->
                let template = Fs.readFileSync(path).toString() |> handleBarsCompile
                templateCache.Add(path, template)
                template
        createObj context |> template

    /// Parses a markdown file
    let parseMarkdown (path: string) =
        Fs.readFileSync(path).toString() |> converter.makeHtml

    /// Parses a React element invoking ReactDOMServer.renderToString
    let parseReact (el: React.ReactElement) =
        ReactDomServer.renderToString el

    /// Parses a React element invoking ReactDOMServer.renderToStaticMarkup
    let parseReactStatic (el: React.ReactElement) =
        ReactDomServer.renderToStaticMarkup el

    let rec private ensureDirExists (dir: string) (cont: (unit->unit) option) =
        if Fs.existsSync !^dir then
            match cont with Some c -> c() | None -> ()
        else
            ensureDirExists (Path.dirname dir) (Some (fun () ->
                if not(Fs.existsSync !^dir) then
                    Fs?mkdirSync(dir) |> ignore
                match cont with Some c -> c() | None -> ()
            ))

    let writeFile (path: string) (content: string) =
        ensureDirExists (Path.dirname path) None
        Fs.writeFileSync(path, content)

    let readFile (path: string) =
        Fs.readFileSync(path).toString()
