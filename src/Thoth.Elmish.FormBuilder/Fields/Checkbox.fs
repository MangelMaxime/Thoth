namespace Thoth.Elmish.FormBuilder.Fields

open Fulma
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Thoth.Elmish
open System

module FormCmd = Thoth.Elmish.FormBuilder.Cmd

[<RequireQualifiedAccess>]
module Checkbox =

    type Key = string

    type CheckboxState =
        { Label : string
          IsChecked : bool }

    type Msg =
        | ToggleState
        interface FormBuilder.Types.IFieldMsg

    let private init (state : FormBuilder.Types.FieldState) =
        state, FormCmd.none

    let private update (msg : FormBuilder.Types.FieldMsg) (state : FormBuilder.Types.FieldState) =
        let msg = msg :?> Msg
        let state = state :?> CheckboxState

        match msg with
        | ToggleState ->
            box { state with IsChecked = not state.IsChecked }, FormCmd.none

    let private render (state : FormBuilder.Types.FieldState) (onChange : FormBuilder.Types.IFieldMsg -> unit) =
        let state : CheckboxState = state :?> CheckboxState
        Field.div [ ]
            [ Control.div [ ]
                [ Checkbox.checkbox [ ]
                    [ Checkbox.input [ Props [ Checked state.IsChecked
                                               OnChange (fun _ ->
                                                onChange ToggleState
                                               ) ] ]
                      span [ ]
                        [ str state.Label ] ] ]
            //   Help.help [ Help.Color IsDanger ]
            //     [ str state.ValidationInputState.ToText ]
                ]

    let config : FormBuilder.Types.FieldConfig =
        { Render = render
          Update = update
          Init = init }

    let create (label : string) : CheckboxState =
        { Label = label
          IsChecked = false }

    let withValue (value : bool ) (state : CheckboxState) =
        { state with IsChecked = value }

    let withDefaultRenderer (state : CheckboxState) : FormBuilder.Types.Field =
        { Type = "default-checkbox"
          State = state
          Guid = Guid.NewGuid() }
