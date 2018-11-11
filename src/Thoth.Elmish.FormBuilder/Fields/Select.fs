namespace Thoth.Elmish.FormBuilder.Fields

open Fulma
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import
open Fable.PowerPack
open Thoth.Elmish
open System

module Select =

    type Key = string

    type SelectState =
        { Label : string
          Placeholder : (Key * string) option
          SelectedKey : Key option
          Values : (Key * string) list
          IsLoading : bool
          ValuesFromServer : JS.Promise<(Key * string) list> option }

    type Msg =
        | ChangeValue of string
        | ReceivedValueFromServer of (Key * string) list
        interface FormBuilder.Types.IFieldMsg

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

    let private init (state : FormBuilder.Types.FieldState) =
        let state = state :?> SelectState

        match state.ValuesFromServer with
        | Some fetchKeyValues ->
            let request () =
                promise {
                    let! keyValues = fetchKeyValues
                    return ReceivedValueFromServer keyValues
                }

            box { state with IsLoading = true }, FormBuilder.Cmd.ofPromise request ()

        | None -> box state, FormBuilder.Cmd.none

    let private update (msg : FormBuilder.Types.FieldMsg) (state : FormBuilder.Types.FieldState) =
        let msg = msg :?> Msg
        let state = state :?> SelectState

        match msg with
        | ChangeValue selectedKey ->
            box { state with SelectedKey = Some selectedKey }, FormBuilder.Cmd.none

        | ReceivedValueFromServer values ->
            box { state with IsLoading = false
                             Values = values }, FormBuilder.Cmd.none

    let private render (state : FormBuilder.Types.FieldState) (onChange : FormBuilder.Types.IFieldMsg -> unit) =
        let state : SelectState = state :?> SelectState
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
            //     [ str state.ValidationSelectState.ToText ]
                 ]

    let config : FormBuilder.Types.FieldConfig =
        { Render = render
          Update = update
          Init = init }


    let create (label : string) : SelectState =
        { Label = label
          Placeholder = None
          SelectedKey = None
          Values = []
          IsLoading = false
          ValuesFromServer = None }

    let withValues (values : (Key * string) list) (state : SelectState) =
        { state with Values = values }

    let withDefaultRenderer (state : SelectState) : FormBuilder.Types.Field =
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
