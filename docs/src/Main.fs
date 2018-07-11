module Main

open Renderer

FromMarkdown.render (Route.Index) "Index.md"
FromMarkdown.render (Route.Json.Encode) "Json/Encode.md"
FromMarkdown.render (Route.Json.Decode) "Json/Decode.md"
FromMarkdown.render (Route.Json.Net) "Json/Net.md"
FromMarkdown.render (Route.Json.Net) "Json/Net.md"
FromMarkdown.render (Route.Elmish.Debouncer) "Elmish/Debouncer.md"
FromMarkdown.render (Route.Elmish.Toast.Docs) "Elmish/Toast_docs.md"
FromMarkdown.render (Route.Elmish.Toast.Demo) "Elmish/Toast_demo.md"
