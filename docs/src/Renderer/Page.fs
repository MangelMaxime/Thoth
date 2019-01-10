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
        | Header of TitleInfo
        | Category of TitleInfo * TitleInfo list

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

    let unescape (s : string) =
        s
            .Replace("&lt;", "<")
            .Replace("&gt;", ">")
            .Replace("&quot;", "\"")
            .Replace("&apos;", "'")
            .Replace("&amp;", "&")

    let tableOfContent (pageBody : string) =
        let titles =
            Regex.Matches(pageBody, "<h([\d])><a href=\"(.*)\" a.*>.*<\/a>(.*)<\/h([\d])>", RegexOptions.IgnoreCase)
            |> Seq.cast<Fable.Import.JS.RegExpExecArray>
            |> Seq.filter (fun item ->
                let level = int item.[1]
                level <= 3
            )
            |> Seq.map (fun item ->
                { Level = int item.[1]
                  Text = unescape item.[3]
                  Href = item.[2] }
            )
            |> Seq.toList

        let rec apply (headings : TitleInfo list) (items : TOC_Item list) =
            match headings with
            | head::tail when head.Level = 1 ->
                apply tail (items @ [ Header head ])

            | head::tail when head.Level = 2 ->
                let subItems =
                    tail
                    |> List.takeWhile (fun heading ->
                        heading.Level = 3
                    )

                apply tail.[subItems.Length..] (items @ [ Category (head, subItems) ])

            | head::tail ->
                printfn "When extracting the TOC, I wasn't able to handle:\n%A\n If you think this is an error, please report an issue" head
                apply tail items

            | [ ] -> items

        let tocItems = apply titles []

        tocItems
        |> List.map (fun item ->
            match item with
            | Header info ->
                div [ Class "toc-label" ]
                    [ a [ Href info.Href ]
                        [ str info.Text ] ]

            | Category (title, children) ->

                let subItems =
                    if children.Length = 0 then
                        unbox null
                    else
                        children
                        |> List.map (fun child ->
                            li [ ]
                                [ a [ Href child.Href ]
                                    [ str child.Text ] ]
                        )
                        |> ul [ Class "toc-sub-headings" ]

                ul [ Class "toc-headings" ]
                    [ li [ ]
                        [ a [ Href title.Href ]
                            [ str title.Text ]
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
