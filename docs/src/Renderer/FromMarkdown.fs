[<RequireQualifiedAccess>]
module Renderer.FromMarkdown

    open Helpers
    open Fable.PowerPack

    let render page filePath =
        let filePath = "${entryDir}/src/Content/" + filePath
        promise {
            let! body = parseMarkdown (resolve filePath)
            do! Page.render {
                PageUrl = page
                Title = None
                Body = body
            }
        }
        |> Promise.start
