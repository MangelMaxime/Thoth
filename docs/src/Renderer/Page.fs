namespace Renderer

[<RequireQualifiedAccess>]
module Page =

    open Docs.Helpers
    open Fable.Core.JsInterop
    open Fulma.Layouts

    type PageConfig =
        { Page : Route.Page
          Title : string option
          Body : string }

    let private footer =
        Footer.footer [ Footer.customClass "has-text-centered" ]
            [ Container.container [ ]
                [ contentFromMarkdown "**Thot** by [Maxime Mangel](https://twitter.com/MangelMaxime)" ]
            ]

    let private templatePath = resolve "${entryDir}/templates/template.hbs"

    let render (config: PageConfig) =
        let title =
            match config.Title with
            | Some title -> "Thot: " + title
            | None -> "Thot"

        let outputFile = resolve ("${entryDir}/public/" + (Route.toUrl config.Page))

        [ "title" ==> title
          "navbar" ==> ((Navbar.render config.Page) |> parseReactStatic)
          "body" ==> config.Body
          "footer" ==> (footer |> parseReactStatic)
        ]
        |> parseTemplate templatePath
        |> writeFile outputFile
