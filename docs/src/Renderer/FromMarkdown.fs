[<RequireQualifiedAccess>]
module Renderer.FromMarkdown

    open Helpers
    open Fable.PowerPack

    let private renderWith (renderer : Page.PageConfig -> Fable.Import.JS.Promise<unit>) (page : string) (filePath : string)  =
        let filePath = "${entryDir}/src/Content/" + filePath
        promise {
            let! body = parseMarkdown (resolve filePath)
            do! renderer {
                PageUrl = page
                Title = None
                Body = body
            }
        }
        |> Promise.start

    let renderNormal = renderWith Page.renderNormal

    let renderWithTOC = renderWith Page.renderWithTOC
