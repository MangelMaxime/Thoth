[<RequireQualifiedAccess>]
module Decode

open Renderer
open Docs.Helpers


open Fulma.Layouts
open Fulma.Elements
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Core

[<Pojo>]
type DangerousInnerHtml =
    { __html : string }

let htmlFromMarkdown str =
    div [ DangerouslySetInnerHTML { __html = makeHtml str } ] [ ]

let private content =
    """
# Decode


    """.Trim()

let body =
    Content.content [ ]
        [ htmlFromMarkdown content ]

let render () =
    Page.render {
        ActivePage = Route.Decode
        Title = None
        Body = body |> parseReactStatic
        OutputFile = "index.html"
    }
