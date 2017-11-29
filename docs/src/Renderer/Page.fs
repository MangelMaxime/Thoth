namespace Renderer

[<RequireQualifiedAccess>]
module Page =

    open Docs.Helpers
    open Fable.Core.JsInterop

    type PageConfig =
        { ActivePage : Navbar.ActivePage
          Title : string option }

    let private templatePath = resolve "${entryDir}/templates/template.hbs"
    let private markdownPath = resolve "${entryDir}/README.md"
    let private indexPath = resolve "${entryDir}/public/index.html"

    let render (config: PageConfig) =
        let title =
            match config.Title with
            | Some title -> "Thot: " + title
            | None -> "Thot"

        [ "title" ==> title
          "navbar" ==> Navbar.render config.ActivePage
          "body" ==> "body content"
        ]
        |> parseTemplate templatePath
        |> writeFile indexPath
