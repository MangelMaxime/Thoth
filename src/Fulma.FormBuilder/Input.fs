namespace Fulma.FormBuilder

open Fulma
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Thoth.Elmish.FormBuilder
open Thoth.Elmish.FormBuilder.Types
open System
open Thoth.Json

[<RequireQualifiedAccess>]
module Input =

    type State =
        { Label : string
          Value : string
          Type : Input.IInputType
          Placeholder : string option
          Validators : Validator list
          ValidationState : ValidationState
          Name : string }

    and Validator = State -> ValidationState

    type Msg =
        | ChangeValue of string
        interface IFieldMsg

    let private init (state : FieldState) =
        state, FormCmd.none

    let private validate (state : FieldState) =
        let state : State = state :?> State
        let rec applyValidators (validators : Validator list) (state : State) =
            match validators with
                | validator::rest ->
                    match validator state with
                    | Valid -> applyValidators rest state
                    | Invalid msg ->
                        { state with ValidationState = Invalid msg }
                | [] -> state

        applyValidators state.Validators { state with ValidationState = Valid } |> box

    let private isValid (state : FieldState) =
        let state : State = state :?> State
        state.ValidationState = Valid

    let private toJson (state : FieldState) =
        let state : State = state :?> State
        state.Name, Encode.string state.Value

    let private update (msg : FieldMsg) (state : FieldState) =
        let msg = msg :?> Msg
        let state = state :?> State

        match msg with
        | ChangeValue newValue ->
            box { state with Value = newValue }, FormCmd.none

    let private view (state : FieldState) (dispatch : IFieldMsg -> unit) =
        let state : State = state :?> State
        let color =
            if isValid state then
                NoColor
            else
                IsDanger

        Field.div [ ]
            [ Label.label [ Label.For state.Name ]
                [ str state.Label ]
              Control.div [ ]
                [ Input.input [ Input.Value state.Value
                                Input.Props [ Id state.Name ]
                                Input.Placeholder (state.Placeholder |> Option.defaultValue "")
                                Input.Type state.Type
                                Input.OnChange (fun ev ->
                                    ev.Value |> ChangeValue |> onChange
                                )
                                Input.Color color ] ]
              Help.help [ Help.Color IsDanger ]
                [ str state.ValidationState.ToText ] ]

    let config : FieldConfig =
        { View = view
          Update = update
          Init = init
          Validate = validate
          IsValid = isValid
          ToJson = toJson }

    let create (name : string) : State =
        { Label = ""
          Value = ""
          Type = Input.Text
          Placeholder = None
          Validators = [ ]
          ValidationState = Valid
          Name = name }

    let withValue (value : string) (state : State) =
        { state with Value = value }

    let withType (typ : Input.IInputType) (state : State) =
        { state with Type = typ }

    let withLabel (label : string ) (state : State) =
        { state with Label = label }

    let withPlaceholder (placeholder : string ) (state : State) =
        { state with Placeholder = Some placeholder }

    let isRequired (msg : String) (state : State) =
        let validator (state : State) =
            if String.IsNullOrWhiteSpace state.Value then
                Invalid msg
            else
                Valid

        { state with Validators = state.Validators @ [ validator ] }

    let withFulmaRenderer (state : State) : Field =
        { Type = "fulma-input"
          State = state
          Guid = Guid.NewGuid() }
