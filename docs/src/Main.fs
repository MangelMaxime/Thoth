module Main

open Renderer

FromMarkdown.renderNormal (Route.Index) "Index.md"

FromMarkdown.renderWithTOC (Route.Json.v1) "Json/v1.md"
FromMarkdown.renderWithTOC (Route.Json.v2) "Json/v2.md"
FromMarkdown.renderWithTOC (Route.Json.v3) "Json/v3.md"

FromMarkdown.renderWithTOC (Route.Elmish.Debouncer) "Elmish/Debouncer.md"
FromMarkdown.renderWithTOC (Route.Elmish.Toast.Docs) "Elmish/Toast_docs.md"
FromMarkdown.renderNormal (Route.Elmish.Toast.Demo) "Elmish/Toast_demo.md"
FromMarkdown.renderWithTOC (Route.Elmish.FormBuilder) "Elmish/FormBuilder.md"
