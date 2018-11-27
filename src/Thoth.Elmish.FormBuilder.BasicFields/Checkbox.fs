namespace Thoth.Elmish.FormBuilder.BasicFields

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Thoth.Elmish.FormBuilder
open Thoth.Elmish.FormBuilder.Types
open System
open Thoth.Json

[<RequireQualifiedAccess>]
module Checkbox =

    type State =
        { Label : string
          IsChecked : bool
          Validators : Validator list
          ValidationState : ValidationState
          Name : string }

    and Validator = State -> ValidationState

    type Msg =
        | ToggleState
        interface IFieldMsg

    let private init (state : FieldState) =
        state, FormCmd.none

    let private validate (state : FieldState) =
        let state = state :?> State
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

    let private setError (state : FieldState) (message : string) =
        let state : State = state :?> State
        { state with ValidationState = Invalid message } |> box

    let private toJson (state : FieldState) =
        let state : State = state :?> State
        state.Name, Encode.bool state.IsChecked

    let private update (msg : FieldMsg) (state : FieldState) =
        let msg = msg :?> Msg
        let state = state :?> State

        match msg with
        | ToggleState ->
            { state with IsChecked = not state.IsChecked }
            |> validate
            |> box, FormCmd.none

    let private view (state : FieldState) (dispatch : IFieldMsg -> unit) =
        let state : State = state :?> State

        div [ Class "field" ]
            [ div [ Class "control" ]
                [ label [ Class "checkbox" ]
                    [ input [ HTMLAttr.Type "checkbox"
                              Class "checkbox"
                              Checked state.IsChecked
                              OnChange (fun _ ->
                                dispatch ToggleState
                              ) ]
                      str state.Label ] ]
              span [ Class "help is-danger" ]
                [ str state.ValidationState.Text ] ]

    let config : FieldConfig =
        { View = view
          Update = update
          Init = init
          Validate = validate
          IsValid = isValid
          ToJson = toJson
          SetError = setError }

type BasicCheckbox private (state : Checkbox.State) =

    member __.WithDefaultView () : FieldBuilder =
        { Type = "basic-checkbrox"
          State = state
          Name = state.Name
          Config = Checkbox.config }

    member __.WithCustomView (view) : FieldBuilder =
        { Type = "basic-checkbrox"
          State = state
          Name = state.Name
          Config = { Checkbox.config with View = view } }

    static member Create (name : string) =
        BasicCheckbox
            { Label = ""
              IsChecked = false
              Validators = [ ]
              ValidationState = Valid
              Name = name }

    member __.WithValue (value : bool) =
        BasicCheckbox { state with IsChecked = value }

    member __.WithLabel (label : string) =
        BasicCheckbox { state with Label = label }

    member __.IsRequired (?msg : string) =
        let msg = defaultArg msg "This field is required"

        let validator (state : Checkbox.State) =
            if state.IsChecked then
                Valid
            else
                Invalid msg

        BasicCheckbox { state with Validators = state.Validators @ [ validator ] }

    member __.AddValidator (validator) =
        BasicCheckbox { state with Validators = state.Validators @ [ validator ] }
