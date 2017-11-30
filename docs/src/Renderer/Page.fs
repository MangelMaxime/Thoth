namespace Renderer

[<RequireQualifiedAccess>]
module Page =

    open Docs.Helpers
    open Fable.Core.JsInterop
    open Fable.Import.React

    type PageConfig =
        { ActivePage : Navbar.ActivePage
          Title : string option
          Body : ReactElement }

    let private templatePath = resolve "${entryDir}/templates/template.hbs"
    let private markdownPath = resolve "${entryDir}/README.md"
    let private indexPath = resolve "${entryDir}/public/index.html"

    let render (config: PageConfig) =
        let title =
            match config.Title with
            | Some title -> "Thot: " + title
            | None -> "Thot"

        [ "title" ==> title
          "navbar" ==> ((Navbar.render config.ActivePage) |> parseReactStatic)
          "body" ==> (config.Body |> parseReactStatic)
        ]
        |> parseTemplate templatePath
        |> writeFile indexPath
