namespace Thoth.Elmish.FormBuilder.Fields

open Fulma
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import
open Fable.PowerPack
open Thoth.Elmish.FormBuilder.Types
open System
open Thoth.Json

module FormCmd = Thoth.Elmish.FormBuilder.Cmd

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

    let private render (state : FieldState) (onChange : IFieldMsg -> unit) =
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
            //   Help.help [ Help.Color IsDanger ]
            //     [ str state.ValidationState.ToText ]
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
        not state.IsLoading || state.ValidationState = Valid

    let private toJson (state : FieldState) =
        let state : State = state :?> State
        state.JsonLabel
            |> Option.defaultValue state.Label, state.SelectedKey
                                                |> Option.map Encode.string
                                                |> Option.defaultValue Encode.nil

    let config : FieldConfig =
        { Render = render
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
        { Type = "default-select"
          State = state
          Guid = Guid.NewGuid() }


    // type SelectBuilder() =

    //     member __.Yield(_) : SelectState =
    //         { Label = ""
    //           SelectedKey = None
    //           JsonLabel = None
    //           Values = []
    //           Validators = []
    //           Placeholder = None
    //           ValidationSelectState = Valid
    //           IsLoading = true
    //           ValuesFromServer = None }

    //     [<CustomOperation("label")>]
    //     member __.Label (selectState : SelectState, label : string) =
    //         { selectState with Label = label }

    //     [<CustomOperation("jsonLabel")>]
    //     member __.JsonLabel (selectState : SelectState, jsonLabel : string) =
    //         { selectState with JsonLabel = Some jsonLabel }

    //     [<CustomOperation("values")>]
    //     member __.Values (selectState : SelectState, values : (Key * string) list) =
    //         { selectState with Values = values
    //                            IsLoading = false }

    //     [<CustomOperation("valuesFromServer")>]
    //     member __.ValuesFromServer (selectState : SelectState, valuesFromServer : JS.Promise<(Key * string) list>) =
    //         { selectState with ValuesFromServer = Some valuesFromServer
    //                            IsLoading = true }

    //     [<CustomOperation("isRequired")>]
    //     member __.IsRequired (selectState : SelectState) =
    //         let apply (selectState : SelectState) =
    //             if selectState.SelectedKey.IsNone then
    //                 Invalid "This field is required"
    //             else
    //                 Valid

    //         { selectState with Validators = selectState.Validators @ [apply] }

    //     [<CustomOperation("placeholder")>]
    //     member __.Placeholder (selectState : SelectState, key, placeholder) =
    //         { selectState with Placeholder = Some (key, placeholder) }

    // let internal appyValidators (selectState : SelectState) =
    //     let rec apply (validators : SelectValidator list) (selectState : SelectState) =
    //         match validators with
    //         | validator::rest ->
    //             match validator selectState with
    //             | Valid -> apply rest selectState
    //             | Invalid msg ->
    //                 { selectState with ValidationSelectState = Invalid msg }
    //         | [] -> selectState

    //     apply selectState.Validators selectState

    // let update (inputState : SelectState) (newValue : Key) =
    //     { inputState with SelectedKey = Some newValue
    //                       ValidationSelectState = Valid }
    //     |> appyValidators

    // let private renderOption (key,value) =
    //     option [ Value key ]
    //         [ str value ]

    // let private renderPlaceHolder (placeholder : (Key * string) option) =
    //     match placeholder with
    //     | Some (key, value) ->
    //         option [ Value key
    //                  Disabled true ]
    //             [ str value ]
    //     | None ->
    //         option [ Disabled true ]
    //             [ ]

    // let render onChange (selectState : SelectState) =
    //     Field.div [ ]
    //         [ Label.label [ ]
    //             [ str selectState.Label ]
    //           Control.div [ ]
    //             [ Select.select [ Select.IsLoading selectState.IsLoading
    //                               Select.IsFullWidth ]
    //                 [ select [ Value (selectState.SelectedKey |> Option.defaultValue "")
    //                            OnChange (fun ev ->
    //                                 ev.Value |> onChange
    //                             ) ]
    //                     [ yield renderPlaceHolder selectState.Placeholder
    //                       yield! (List.map renderOption selectState.Values) ] ] ]
    //           Help.help [ Help.Color IsDanger ]
    //             [ str selectState.ValidationSelectState.ToText ] ]
