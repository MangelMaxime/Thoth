namespace Fulma.FormBuilder

open Thoth.Elmish

module Config =

    let config =
        Map.empty<FormBuilder.Types.FieldType, FormBuilder.Types.FieldConfig>
        |> Map.add "fulma-input"
            { Render = Input.render
              Update = Input.update
              Init = Input.init }
        |> Map.add "fulma_select"
            { Render = Select.render
              Update = Select.update
              Init = Select.init }
