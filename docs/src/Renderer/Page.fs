[<RequireQualifiedAccess>]
module Renderer.Page

    open Helpers
    open Fable.Core.JsInterop
    open Fulma
    open Fable.Helpers.React
    open Fable.Helpers.React.Props
    open Fable.PowerPack
    open System.Text.RegularExpressions

    type TitleInfo =
        { Level : int
          Text : string
          Href : string }

    type TOC_Item =
        { Parent : TitleInfo
          Children : TitleInfo list }


    type PageConfig =
        { PageUrl : string
          Title : string option
          Body : string }

    let private footer _ =
        promise {
            let! content =
                contentFromMarkdown
                        """
**Thoth** by [Maxime Mangel](https://twitter.com/MangelMaxime)

Powered by [Fulma](https://mangelmaxime.github.io/Fulma/), [Fable static-web-generator](https://github.com/fable-compiler/static-web-generator) and [Code-Lightner](https://github.com/MangelMaxime/Code-Lightner).
                        """

            return div [ ]
                [ br [ ]
                  Footer.footer [ CustomClass "has-text-centered" ]
                    [ Container.container [ ]
                        [ content ]
                    ] ]
        }

    module private Template =
        let normal = resolve "${entryDir}/templates/template.hbs"
        let withTOC = resolve "${entryDir}/templates/template_with_toc.hbs"

    let tableOfContent (pageBody : string) =
        let titles =
            Regex.Matches(pageBody, "<h([\d])><a href=\"(.*)\" a.*>.*<\/a>(.*)<\/h([\d])>", RegexOptions.IgnoreCase)
            |> Seq.cast<Fable.Import.JS.RegExpExecArray>
            |> Seq.filter (fun item ->
                let level = int item.[1]
                level = 2 || level = 3
            )
            |> Seq.map (fun item ->
                { Level = int item.[1]
                  Text = item.[3]
                  Href = item.[2] }
            )
            |> Seq.toList

        let rec apply (titles : TitleInfo list) (items : TOC_Item list) =
            match titles with
            | head::tail ->
                let subItems =
                    tail
                    |> List.takeWhile (fun title ->
                        title.Level = 3
                    )

                let mainItem =
                    { Parent = head
                      Children = subItems }

                apply (tail.[subItems.Length..]) (items @ [ mainItem ])
            | [ ] -> items

        let tocItems = apply titles []

        tocItems
        |> List.map (fun item ->
            let subItems =
                if item.Children.Length = 0 then
                    unbox null
                else
                    item.Children
                    |> List.map (fun child ->
                        li [ ]
                            [ a [ Href child.Href ]
                                [ str child.Text ] ]
                    )
                    |> ul [ Class "toc-sub-headings" ]

            ul [ Class "toc-headings" ]
                [ li [ ]
                    [ a [ Href item.Parent.Href ]
                        [ str item.Parent.Text ]
                      subItems ] ]
        )
        |> nav [ Class "toc" ]
        |> parseReactStatic

    let private renderWith (config: PageConfig) templatePath =
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

        promise {
            let! footer = footer ()
            [ "title" ==> title
              "styles" ==> styleTags
              "navbar" ==> ((Components.Navbar.render config.PageUrl) |> parseReactStatic)
              "body" ==> config.Body
              "toc" ==> tableOfContent config.Body
              "footer" ==> (footer |> parseReactStatic)
            ]
            |> parseTemplate templatePath
            |> writeFile outputFile
        }

    let renderNormal (pageConfig : PageConfig) = renderWith pageConfig Template.normal
    let renderWithTOC (pageConfig : PageConfig) = renderWith pageConfig Template.withTOC
