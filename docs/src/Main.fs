module Main

open Renderer

FromMarkdown.render Route.Index "Index.md"
FromMarkdown.render Route.Encode "Encode.md"
FromMarkdown.render Route.Decode "Decode.md"
