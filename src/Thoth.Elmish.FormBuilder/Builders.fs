namespace Thoth.Elmish.FormBuilder

[<AutoOpen>]
module Builders =

    let form = new Form.FormBuilder()
    let input = new Input.InputBuilder()
    let select = new Select.SelectBuilder()
    let button = new Button.ButtonBuilder()
