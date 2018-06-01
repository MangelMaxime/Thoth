[<RequireQualifiedAccess>]
module Renderer.Page

    open Helpers
    open Fable.Core.JsInterop
    open Fulma
    open Fable.Helpers.React
    open Fable.Helpers.React.Props

    type PageConfig =
        { PageUrl : string
          Title : string option
          Body : string }

    let private footer =
        div [ ]
            [ br [ ]
              Footer.footer [ CustomClass "has-text-centered" ]
                [ Container.container [ ]
                    [ contentFromMarkdown
                        """
**Thoth** by [Maxime Mangel](https://twitter.com/MangelMaxime)

Powered by [Fulma](https://mangelmaxime.github.io/Fulma/) and [Fable static-page-generator](https://github.com/fable-compiler/static-page-generator).
                        """ ]
                ] ]

    let private templatePath = resolve "${entryDir}/templates/template.hbs"

    let render (config: PageConfig) =
        let title =
            match config.Title with
            | Some title -> "Thoth: " + title
            | None -> "Thoth"

        // Local ref to host in order to access the string length
        let host = Route.Host
        // Remove the host part of the url
        let outputFile = resolve ("${entryDir}/public/" + config.PageUrl.Substring(host.Length))

        let styleTags =
            [ "main.css" ]
            |> List.mapi(fun index styleUrl ->
                link [ Rel "stylesheet"
                       Type "text/css"
                       Key ("style-" + string index)
                       Href (Route.Host + styleUrl) ]
            )
            |> ofList
            |> parseReactStatic

        [ "title" ==> title
          "styles" ==> styleTags
          "navbar" ==> ((Components.Navbar.render config.PageUrl) |> parseReactStatic)
          "body" ==> config.Body
          "footer" ==> (footer |> parseReactStatic)
        ]
        |> parseTemplate templatePath
        |> writeFile outputFile
