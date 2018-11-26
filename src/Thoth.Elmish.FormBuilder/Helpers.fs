namespace Thoth.Elmish.FormBuilder

module internal Helpers =

    open Fable.Import

    /// Add the provided `css` to the page if not already added
    let appendStyle (id : string) (css : string) =
        if isNull (Browser.document.getElementById(id)) then
            let node = Browser.document.createElement_style()
            node.textContent <- css
            node.``type`` <- "text/css"
            node.id <- id

            Browser.document.head.appendChild(node)
            |> ignore
