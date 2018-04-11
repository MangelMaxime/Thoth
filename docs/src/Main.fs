module Main

open Renderer

FromMarkdown.render (Route.Index) "Index.md"
FromMarkdown.render (Route.Json.Encode) "Json/Encode.md"
FromMarkdown.render (Route.Json.Decode) "Json/Decode.md"
FromMarkdown.render (Route.Json.Net) "Json/Net.md"
