namespace Thoth.Elmish.FormBuilder.BasicFields

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
          Name : string }

    and Validator = State -> ValidationState

    type Msg =
        | ChangeValue of string
        | ReceivedValueFromServer of (Key * string) list
        | OnError of exn
        interface IFieldMsg

    let private init (state : FieldState) =
        let state = state :?> State

        match state.ValuesFromServer with
        | Some fetchKeyValues ->
            let request () =
                promise {
                    return! fetchKeyValues
                }

            box { state with IsLoading = true }, FormCmd.ofPromise request () ReceivedValueFromServer OnError

        | None -> box state, FormCmd.none

    let private isValid (state : FieldState) =
        let state : State = state :?> State
        not state.IsLoading && state.ValidationState = Valid

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

    let private setError (state : FieldState) (message : string) =
        let state : State = state :?> State
        { state with ValidationState = Invalid message } |> box

    let private toJson (state : FieldState) =
        let state : State = state :?> State
        state.Name, state.SelectedKey
                        |> Option.map Encode.string
                        |> Option.defaultValue Encode.nil

    let private update (msg : FieldMsg) (state : FieldState) =
        let msg = msg :?> Msg
        let state = state :?> State

        match msg with
        | ChangeValue selectedKey ->
            { state with SelectedKey = Some selectedKey }
            |> validate
            |> box, FormCmd.none

        | ReceivedValueFromServer values ->
            box { state with IsLoading = false
                             Values = values }, FormCmd.none

        | OnError error ->
            Browser.console.error error
            box state, FormCmd.none

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
            nothing

    let private view (state : FieldState) (dispatch : IFieldMsg -> unit) =
        let state : State = state :?> State

        let selectClass =
            // Don't display the select in red when the values are being loading from the server
            [ "is-danger", not state.IsLoading && not (isValid state)
              "is-loading", state.IsLoading ]
            |> classBaseList "select is-fullwidth"

        let placeholderKey =
            state.Placeholder
            |> Option.map fst
            |> Option.defaultValue ""

        div [ Class "field" ]
            [ label [ Class "label"
                      HtmlFor state.Name ]
                [ str state.Label ]
              div [ Class "control" ]
                [ div [ selectClass ]
                    [ select [ Value (state.SelectedKey |> Option.defaultValue placeholderKey)
                               OnChange (fun ev ->
                                    ev.Value |> ChangeValue |> dispatch
                                ) ]
                        [ renderPlaceHolder state.Placeholder
                          state.Values
                          |> List.map renderOption
                          |> ofList ] ] ]
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

type BasicSelect private (state : Select.State) =

    member __.WithDefaultView () : FieldBuilder =
        { Type = "basic-select"
          State = state
          Name = state.Name
          Config = Select.config }

    member __.WithCustomView (view) : FieldBuilder =
        { Type = "basic-select"
          State = state
          Name = state.Name
          Config = { Select.config with View = view } }

    static member Create(name : string) =
        BasicSelect
            { Label = ""
              Placeholder = None
              SelectedKey = None
              Values = []
              IsLoading = false
              ValuesFromServer = None
              Validators = [ ]
              ValidationState = Valid
              Name = name }

    member __.WithLabel (label : string) : BasicSelect =
        BasicSelect { state with Label = label }

    member __.WithPlaceholder (placeholder : string, ?key : Select.Key) : BasicSelect =
        let key = defaultArg key "this-is-the-placeholder-value"
        BasicSelect { state with Placeholder = Some (key, placeholder) }

    member __.WithSelectedKey (key : Select.Key) : BasicSelect =
        BasicSelect { state with SelectedKey = Some key }

    member __.WithValues (values : (Select.Key * string) list) : BasicSelect =
        BasicSelect { state with Values = values }

    member __.WithValuesFromServer (request : JS.Promise<(Select.Key * string) list>) : BasicSelect =
        BasicSelect { state with ValuesFromServer = Some request
                                 IsLoading = true }

    member __.IsRequired (?msg : string) =
        let msg = defaultArg msg "This field is required"

        let validator (state : Select.State) =
            if Option.isSome state.SelectedKey then
                Valid
            else
                Invalid msg

        BasicSelect { state with Validators = state.Validators @ [ validator ] }

    member __.AddValidator (validator) =
        BasicSelect { state with Validators = state.Validators @ [ validator ] }
