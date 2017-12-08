[<RequireQualifiedAccess>]
module Renderer.FromMarkdown

    open Helpers

    let render page filePath =
        let filePath = "${entryDir}/src/Content/" + filePath
        Page.render {
            PageUrl = page
            Title = None
            Body = parseMarkdown (resolve filePath)
        }
