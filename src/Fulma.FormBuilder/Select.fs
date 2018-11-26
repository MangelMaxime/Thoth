namespace Fulma.FormBuilder

open Fulma
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import
open Fable.PowerPack
open Thoth.Elmish.FormBuilder
open Thoth.Elmish.FormBuilder.Types
open System
open Thoth.Json

module Select =

    type Key = string

    type State =
        { Label : string
          Placeholder : (Key * string) option
          SelectedKey : Key option
          Values : (Key * string) list
          IsLoading : bool
          ValuesFromServer : JS.Promise<(Key * string) list> option
          Validators : Validator list
          ValidationState : ValidationState
          JsonLabel : string option }

    and Validator = State -> ValidationState

    type Msg =
        | ChangeValue of string
        | ReceivedValueFromServer of (Key * string) list
        interface IFieldMsg

    let private renderOption (key,value) =
        option [ Value key
                 Prop.Key key ]
            [ str value ]

    let private renderPlaceHolder (placeholder : (Key * string) option) =
        match placeholder with
        | Some (key, value) ->
            option [ Value key
                     Disabled true ]
                [ str value ]
        | None ->
            option [ Disabled true ]
                [ ]

    let private init (state : FieldState) =
        let state = state :?> State

        match state.ValuesFromServer with
        | Some fetchKeyValues ->
            let request () =
                promise {
                    let! keyValues = fetchKeyValues
                    return ReceivedValueFromServer keyValues
                }

            box { state with IsLoading = true }, FormCmd.ofPromise request ()

        | None -> box state, FormCmd.none

    let private update (msg : FieldMsg) (state : FieldState) =
        let msg = msg :?> Msg
        let state = state :?> State

        match msg with
        | ChangeValue selectedKey ->
            box { state with SelectedKey = Some selectedKey }, FormCmd.none

        | ReceivedValueFromServer values ->
            box { state with IsLoading = false
                             Values = values }, FormCmd.none

    let private view (state : FieldState) (dispatch : IFieldMsg -> unit) =
        let state : State = state :?> State
        Field.div [ ]
            [ Label.label [ ]
                [ str state.Label ]
              Control.div [ ]
                [ Select.select [ Select.IsLoading state.IsLoading
                                  Select.IsFullWidth ]
                    [ select [ Value (state.SelectedKey |> Option.defaultValue "")
                               OnChange (fun ev ->
                                    ev.Value |> ChangeValue |> onChange
                                ) ]
                        [ renderPlaceHolder state.Placeholder
                          state.Values
                          |> List.map renderOption
                          |> ofList ] ] ]
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
        not state.IsLoading && state.ValidationState = Valid

    let private toJson (state : FieldState) =
        let state : State = state :?> State
        state.JsonLabel
        |> Option.defaultValue state.Label, state.SelectedKey
                                            |> Option.map Encode.string
                                            |> Option.defaultValue Encode.nil

    let config : FieldConfig =
        { View = view
          Update = update
          Init = init
          Validate = validate
          IsValid = isValid
          ToJson = toJson }

    let create (label : string) : State =
        { Label = label
          Placeholder = None
          SelectedKey = None
          Values = []
          IsLoading = false
          ValuesFromServer = None
          Validators = [ ]
          ValidationState = Valid
          JsonLabel = None }

    let withSelectedKey (key : Key ) (state : State) =
        { state with SelectedKey = Some key }

    let withValues (values : (Key * string) list) (state : State) =
        { state with Values = values }

    let withDefaultRenderer (state : State) : Field =
        { Type = "fulma-select"
          State = state
          Guid = Guid.NewGuid() }
