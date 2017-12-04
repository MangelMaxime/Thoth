namespace Renderer

[<RequireQualifiedAccess>]
module Page =

    open Docs.Helpers
    open Fable.Core.JsInterop
    open Fulma.Layouts
    open Fable.Helpers.React

    type PageConfig =
        { Page : Route.Page
          Title : string option
          Body : string }

    let private footer =
        div [ ]
            [ br [ ]
              Footer.footer [ Footer.customClass "has-text-centered" ]
                [ Container.container [ ]
                    [ contentFromMarkdown "**Thot** by [Maxime Mangel](https://twitter.com/MangelMaxime)" ]
                ] ]

    #if DEBUG
    let private templatePath = resolve "${entryDir}/templates/template.dev.hbs"
    #else
    let private templatePath = resolve "${entryDir}/templates/template.prod.hbs"
    #endif

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
