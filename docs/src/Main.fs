module Main

open Renderer

FromMarkdown.renderNormal (Route.Index) "Index.md"

FromMarkdown.renderWithTOC (Route.Json.v1.Encode) "Json/v1/Encode.md"
FromMarkdown.renderWithTOC (Route.Json.v1.Decode) "Json/v1/Decode.md"
FromMarkdown.renderWithTOC (Route.Json.v1.Net) "Json/v1/Net.md"

FromMarkdown.renderWithTOC (Route.Json.v2.Encode) "Json/v2/Encode.md"
FromMarkdown.renderWithTOC (Route.Json.v2.Decode) "Json/v2/Decode.md"
FromMarkdown.renderWithTOC (Route.Json.v2.Net) "Json/v2/Net.md"

FromMarkdown.renderWithTOC (Route.Elmish.Debouncer) "Elmish/Debouncer.md"
FromMarkdown.renderWithTOC (Route.Elmish.Toast.Docs) "Elmish/Toast_docs.md"
FromMarkdown.renderNormal (Route.Elmish.Toast.Demo) "Elmish/Toast_demo.md"
FromMarkdown.renderWithTOC (Route.Elmish.FormBuilder) "Elmish/FormBuilder.md"
