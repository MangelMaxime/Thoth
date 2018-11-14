namespace Thoth.Elmish.FormBuilder.Fields

open Fulma
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
          JsonLabel : string option }

    and Validator = State -> ValidationState

    type Msg =
        | ToggleState
        interface IFieldMsg

    let private init (state : FieldState) =
        state, FormCmd.none

    let private update (msg : FieldMsg) (state : FieldState) =
        let msg = msg :?> Msg
        let state = state :?> State

        match msg with
        | ToggleState ->
            { state with IsChecked = not state.IsChecked }
            |> toFieldState, FormCmd.none

    let private render (state : FieldState) (onChange : IFieldMsg -> unit) =
        let state : State = state :?> State
        Field.div [ ]
            [ Control.div [ ]
                [ Checkbox.checkbox [ ]
                    [ Checkbox.input [ Props [ Checked state.IsChecked
                                               OnChange (fun _ ->
                                                onChange ToggleState
                                               ) ] ]
                      str state.Label ] ]
            //   Help.help [ Help.Color IsDanger ]
            //     [ str state.ValidationInputState.ToText ]
                ]

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

        applyValidators state.Validators state
        |> toFieldState

    let private isValid (state : FieldState) =
        let state : State = state :?> State
        state.ValidationState = Valid

    let private toJson (state : FieldState) =
        let state : State = state :?> State
        state.JsonLabel
        |> Option.defaultValue state.Label, Encode.bool state.IsChecked

    let config : FieldConfig =
        { Render = render
          Update = update
          Init = init
          Validate = validate
          IsValid = isValid
          ToJson = toJson }

    let create (label : string) : State =
        { Label = label
          IsChecked = false
          Validators = [ ]
          ValidationState = Valid
          JsonLabel = None }

    let withValue (value : bool ) (state : State) =
        { state with IsChecked = value }

    let withDefaultRenderer (state : State) : Field =
        { Type = "default-checkbox"
          State = state
          Guid = Guid.NewGuid() }
