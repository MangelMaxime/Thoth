namespace Thoth.Elmish.FormBuilder

open Thoth.Json
open Fable.Import
open Elmish

module Types =

    /// Error representation to support server side validation
    type ErrorDef =
        { Text : string
          Key : string }

        static member Decoder =
            Decode.object
                (fun get ->
                    { Text = get.Required.Field "text" Decode.string
                      Key = get.Required.Field "key" Decode.string } : ErrorDef )

        static member Encoder (error : ErrorDef) =
            Encode.object [
                "text", Encode.string error.Text
                "key", Encode.string error.Key
            ]

    /// Used to describe if a field is `Valid` or `Invalid` with the message to display
    type ValidationState =
        | Valid
        | Invalid of string

        member this.Text
            with get () =
                match this with
                | Valid -> ""
                | Invalid msg -> msg

    /// Interface to be implemented by any field `Msg`
    type IFieldMsg =
        interface end

    /// Type alias to identify a field type in the `Config`
    /// Needs to be unique per field type in your `Config`
    type FieldType = string

    /// Type alias for the field `State`, should be casted
    type FieldState = obj

    /// Type alias for the field `Msg`, should be casted
    type FieldMsg = obj

    /// Field identifier
    /// The name should be unique per form instance
    type FieldName = string

    /// Record to register a field
    type Field =
        { /// Type alias to identify the type of the field
          Type : FieldType
          /// Current state of the field in the form
          State : FieldState
          /// Unique Id of the field in the form
          Name : FieldName }

    type Msg =
        // | DebouncerSelfMsg of Debouncer.SelfMessage<Msg>
        | OnFieldMessage of FieldName * FieldState

    /// Track current state of the Form
    type State =
        { Fields : Field list
          IsLoading : bool }

    /// Contract for registering fields in the `Config`
    type FieldConfig =
        { View : FieldState -> (IFieldMsg -> unit) -> React.ReactElement
          Update : FieldMsg -> FieldState -> FieldState * (string -> Cmd<Msg>)
          Init : FieldState -> FieldState * (string -> Cmd<Msg>)
          Validate : FieldState -> FieldState
          IsValid : FieldState -> bool
          ToJson : FieldState -> string * JsonValue
          SetError : FieldState -> string -> FieldState }

    /// Configuration for the Form
    type Config<'AppMsg> =
        { ToMsg : Msg -> 'AppMsg
          FieldsConfig : Map<FieldType, FieldConfig> }

    type FieldBuilder =
        { Type : FieldType
          State : FieldState
          Name : FieldName
          Config : FieldConfig }
