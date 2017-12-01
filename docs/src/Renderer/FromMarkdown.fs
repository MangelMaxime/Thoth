namespace Renderer

[<RequireQualifiedAccess>]
module FromMarkdown =

    open Renderer
    open Docs.Helpers

    open Fulma.Elements
    open Fable.Helpers.React

    let render page filePath =
        let filePath = "${entryDir}/src/Content/" + filePath
        Page.render {
            Page = page
            Title = None
            Body = parseMarkdown (resolve filePath)
        }
