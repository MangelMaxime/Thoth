namespace Fulma.FormBuilder

open Fulma
open Fable.Helpers.React
open Thoth.Elmish.FormBuilder
open Thoth.Elmish.FormBuilder.Types
open System
open Thoth.Json

[<RequireQualifiedAccess>]
module Textarea =

    type State =
        { Label : string
          Value : string
          Validators : Validator list
          ValidationState : ValidationState
          JsonLabel : string option }

    and Validator = State -> ValidationState

    type Msg =
        | ChangeValue of string
        interface IFieldMsg

    let private init (state : FieldState) =
        state, FormCmd.none

    let private update (msg : FieldMsg) (state : FieldState) =
        let msg = msg :?> Msg
        let state = state :?> State

        match msg with
        | ChangeValue newValue ->
            box { state with Value = newValue }, FormCmd.none

    let private view (state : FieldState) (dispatch : IFieldMsg -> unit) =
        let state : State = state :?> State
        Field.div [ ]
            [ Label.label [ ]
                [ str state.Label ]
              Control.div [ ]
                [ Textarea.textarea [ Textarea.Value state.Value
                                      Textarea.OnChange (fun ev ->
                                        ev.Value |> ChangeValue |> onChange
                                      ) ]
                    [ ] ]
              Help.help [ Help.Color IsDanger ]
                [ str state.ValidationState.ToText ] ]

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
        state.JsonLabel
        |> Option.defaultValue state.Label, Encode.string state.Value

    let config : FieldConfig =
        { View = view
          Update = update
          Init = init
          Validate = validate
          IsValid = isValid
          ToJson = toJson }

    let create (label : string) : State =
        { Label = label
          Value = ""
          Validators = [ ]
          ValidationState = Valid
          JsonLabel = None }

    let withValue (value : string) (state : State) =
        { state with Value = value }

    let withDefaultRenderer (state : State) : Field =
        { Type = "fulma-textarea"
          State = state
          Guid = Guid.NewGuid() }
