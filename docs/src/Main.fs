module Main

open Renderer

FromMarkdown.render Route.Index "Index.md"
FromMarkdown.render (Route.Json Route.Encode) "Encode.md"
FromMarkdown.render (Route.Json Route.Decode) "Decode.md"
