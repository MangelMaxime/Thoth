module Main

open Renderer

FromMarkdown.render (Route.Index) "Index.md"

FromMarkdown.render (Route.Json.v1.Encode) "Json/v1/Encode.md"
FromMarkdown.render (Route.Json.v1.Decode) "Json/v1/Decode.md"
FromMarkdown.render (Route.Json.v1.Net) "Json/v1/Net.md"

FromMarkdown.render (Route.Json.v2.Encode) "Json/v2/Encode.md"
FromMarkdown.render (Route.Json.v2.Decode) "Json/v2/Decode.md"
FromMarkdown.render (Route.Json.v2.Net) "Json/v2/Net.md"

FromMarkdown.render (Route.Elmish.Debouncer) "Elmish/Debouncer.md"
FromMarkdown.render (Route.Elmish.Toast.Docs) "Elmish/Toast_docs.md"
FromMarkdown.render (Route.Elmish.Toast.Demo) "Elmish/Toast_demo.md"
