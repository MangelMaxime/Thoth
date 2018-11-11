namespace Thoth.Elmish.FormBuilder.Fields

open Fulma
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Thoth.Elmish
open System

module FormCmd = Thoth.Elmish.FormBuilder.Cmd

[<RequireQualifiedAccess>]
module RadioButton =

    type Key = string

    type State =
        { Label : string
          SelectedKey : Key option
          Values : (Key * string) list
          Group : string }

    type Msg =
        | ChangeValue of string
        interface FormBuilder.Types.IFieldMsg

    let private init (state : FormBuilder.Types.FieldState) =
        state, FormCmd.none

    let private update (msg : FormBuilder.Types.FieldMsg) (state : FormBuilder.Types.FieldState) =
        let msg = msg :?> Msg
        let state = state :?> State

        match msg with
        | ChangeValue key ->
            box { state with SelectedKey = Some key }, FormCmd.none

    let private renderRadio (group : string) (selectedKey : Key option) (onChange : FormBuilder.Types.IFieldMsg -> unit) (key, value) =
        Radio.radio [ ]
            [ Radio.input [ Radio.Input.Props [ OnChange (fun _ -> ChangeValue key |> onChange)
                                                selectedKey
                                                |> Option.map (fun cKey -> cKey = key)
                                                |> Option.defaultValue false
                                                |> Checked
                                                Prop.Key key ]
                            Radio.Input.Name group ]
              str value ]

    let private render (state : FormBuilder.Types.FieldState) (onChange : FormBuilder.Types.IFieldMsg -> unit) =
        let state : State = state :?> State

        Field.div [ ]
            [ Label.label [ ]
                [ str state.Label ]
              Control.div [ ]
                [ state.Values
                  |> List.map (renderRadio state.Group state.SelectedKey onChange)
                  |> ofList ]
            //   Help.help [ Help.Color IsDanger ]
            //     [ str state.ValidationInputState.ToText ]
                ]

    let config : FormBuilder.Types.FieldConfig =
        { Render = render
          Update = update
          Init = init }

    let create (label : string) : State =
        { Label = label
          SelectedKey = None
          Values = []
          Group = (Guid.NewGuid()).ToString() }

    let withValues (values : (Key * string) list) (state : State) =
        { state with Values = values }

    let withSelectedKey (key : Key ) (state : State) =
        { state with SelectedKey = Some key }

    let withDefaultRenderer (state : State) : FormBuilder.Types.Field =
        { Type = "default-radio-button"
          State = state
          Guid = Guid.NewGuid() }
