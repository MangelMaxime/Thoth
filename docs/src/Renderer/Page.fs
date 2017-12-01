namespace Renderer

[<RequireQualifiedAccess>]
module Page =

    open Docs.Helpers
    open Fable.Core.JsInterop
    open Fable.Import.React

    type PageConfig =
        { ActivePage : Navbar.ActivePage
          Title : string option
          Body : string
          OutputFile : string }

    let private templatePath = resolve "${entryDir}/templates/template.hbs"
    let private markdownPath = resolve "${entryDir}/README.md"
    let private indexPath = resolve "${entryDir}/public/index.html"

    let render (config: PageConfig) =
        let title =
            match config.Title with
            | Some title -> "Thot: " + title
            | None -> "Thot"

        let outputFile = resolve ("${entryDir}/public/" + config.OutputFile)

        [ "title" ==> title
          "navbar" ==> ((Navbar.render config.ActivePage) |> parseReactStatic)
          "body" ==> config.Body
        ]
        |> parseTemplate templatePath
        |> writeFile outputFile
