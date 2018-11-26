namespace Fulma.FormBuilder

open Thoth.Elmish.FormBuilder

[<AutoOpen>]
module Prelude =

    let fulmaFieldsConfig =
        Map.empty<Types.FieldType, Types.FieldConfig>
        |> Map.add "fulma-input" Input.config
        |> Map.add "fulma-select" Select.config
        |> Map.add "fulma-checkbox" Checkbox.config
        |> Map.add "fulma-textarea" Textarea.config
        |> Map.add "fulma-radio-button" RadioButton.config
