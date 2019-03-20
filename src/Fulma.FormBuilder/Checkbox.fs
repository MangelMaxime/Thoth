namespace Fulma.FormBuilder

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Thoth.Elmish.FormBuilder
open Thoth.Elmish.FormBuilder.Types
open Thoth.Json
open Fulma

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
        let state : State = state :?> State
        let rec applyValidators (validators : Validator list) (state : State) =
            match validators with
                | validator::rest ->
                    match validator state with
                    | Valid -> applyValidators rest state
                    | Invalid msg ->
                        { state with ValidationState = Invalid msg }
                | [] -> state

        applyValidators state.Validators state |> box

    let private isValid (state : FieldState) =
        let state : State = state :?> State
        state.ValidationState = Valid

    let private toJson (state : FieldState) =
        let state : State = state :?> State
        state.Name, Encode.bool state.IsChecked

    let private update (msg : FieldMsg) (state : FieldState) =
        let msg = msg :?> Msg
        let state = state :?> State

        match msg with
        | ToggleState ->
            box { state with IsChecked = not state.IsChecked }, FormCmd.none

    let private view (state : FieldState) (dispatch : IFieldMsg -> unit) =
        let state : State = state :?> State
        Field.div [ ]
            [ Control.div [ ]
                [ Checkbox.checkbox [ ]
                    [ Checkbox.input [ Props [ Checked state.IsChecked
                                               OnChange (fun _ ->
                                                dispatch ToggleState
                                               ) ] ]
                      str state.Label ] ]
              Help.help [ Help.Color IsDanger ]
                [ str state.ValidationState.Text ] ]

    let config : FieldConfig =
        { View = view
          Update = update
          Init = init
          SetError = failwith "not implemented"
          Validate = validate
          IsValid = isValid
          ToJson = toJson }

    let create (name : string) : State =
        { Label = ""
          IsChecked = false
          Validators = [ ]
          ValidationState = Valid
          Name = name }

    let withValue (value : bool ) (state : State) =
        { state with IsChecked = value }

    let withLabel (label : string ) (state : State) =
        { state with Label = label }

    let withDefaultRenderer (state : State) : Field =
        { Type = "fulma-checkbox"
          State = state
          Name = state.Name }
