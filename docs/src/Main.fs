module Main

open Renderer

FromMarkdown.render (Route.Index) "Index.md"
FromMarkdown.render (Route.Json.Encode) "Json/Encode.md"
FromMarkdown.render (Route.Json.Decode) "Json/Decode.md"
FromMarkdown.render (Route.Http.Basic) "Http/Basic.md"
FromMarkdown.render (Route.Http.Elmish) "Http/Elmish.md"
