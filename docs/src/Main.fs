namespace Docs

module Main =

    open Fable.Core.JsInterop
    open Renderer


    Page.render {
        ActivePage = Navbar.Decode
        Title = None
    }
