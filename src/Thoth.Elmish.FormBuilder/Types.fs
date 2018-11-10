namespace Thoth.Elmish.FormBuilder

open Thoth.Elmish
open Fable.Import
open Elmish

module Types =

    type ValidationState=
        | Valid
        | Invalid of string

        member this.ToText
            with get () =
                match this with
                | Valid -> ""
                | Invalid msg -> msg

    type IFieldMsg =
        interface end

    /// Type alias to identify a field type in the `Config`
    /// Needs to be unique per field type in your `Config`
    type FieldType = string

    /// Type alias to identify a field in a form
    /// Needs to be unique per field
    type FieldId = string

    /// Type alias for the field `State`, should be casted
    type FieldState = obj

    /// Type alias for the field `Msg`, should be casted
    type FieldMsg = obj

    /// Record to register a field
    type Field =
        { /// Type alias to identify the type of the field
          Type : FieldType
          /// Current state of the field in the form
          State : FieldState
          /// Unique Id of the field in the form
          Id : FieldId }

    type Msg =
        | DebouncerSelfMsg of Debouncer.SelfMessage<Msg>
        | OnFieldMessage of FieldId * FieldState

    type Form<'AppMsg> =
        { Fields : Field list
          OnFormMsg : Msg -> 'AppMsg }

    /// Record to config a field behavior
    type FieldConfig =
        { Render : FieldState -> (IFieldMsg -> unit) -> React.ReactElement
          Update : FieldMsg -> FieldState -> FieldState * (FieldId -> Cmd<Msg>)
          Init : FieldState -> FieldState * (FieldId -> Cmd<Msg>) }

    type Config = Map<FieldType, FieldConfig>
