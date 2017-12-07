module Main

open Renderer

FromMarkdown.render (Route.Index) "Index.md"
FromMarkdown.render (Route.Json.Encode) "Encode.md"
FromMarkdown.render (Route.Json.Decode) "Decode.md"
