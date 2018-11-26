namespace Thoth.Elmish.FormBuilder.BasicFields

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Thoth.Elmish.FormBuilder
open Thoth.Elmish.FormBuilder.Types
open System
open Thoth.Json

[<RequireQualifiedAccess>]
module Radio =

    type Key = string

    type State =
        { Label : string
          SelectedKey : Key option
          Values : (Key * string) list
          Group : string
          Validators : Validator list
          ValidationState : ValidationState
          Name : string }

    and Validator = State -> ValidationState

    type Msg =
        | ChangeValue of string
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

        applyValidators state.Validators { state with ValidationState = Valid } |> box

    let private isValid (state : FieldState) =
        let state : State = state :?> State
        state.ValidationState = Valid

    let private setError (state : FieldState) (message : string)=
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
        | ChangeValue key ->
            { state with SelectedKey = Some key }
            |> validate
            |> box, FormCmd.none

    let private renderRadio (group : string) (selectedKey : Key option) (dispatch : IFieldMsg -> unit) (key, value) =
        let radioId = group + "-" + key
        label [ Class "radio"
                HtmlFor radioId
                Prop.Key key ]
            [ input [ HTMLAttr.Type "radio"
                      Class "radio"
                      Id radioId
                      OnChange (fun _ -> ChangeValue key |> dispatch)
                      selectedKey
                      |> Option.map (fun cKey -> cKey = key)
                      |> Option.defaultValue false
                      |> Checked ]
              str value ]

    let private view (state : FieldState) (dispatch : IFieldMsg -> unit) =
        let state : State = state :?> State

        div [ Class "field" ]
            [ label [ Class "label" ]
                [ str state.Label ]
              div [ Class "control" ]
                [ state.Values
                  |> List.map (renderRadio state.Group state.SelectedKey dispatch)
                  |> ofList ]
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

type BasicRadio private (state : Radio.State) =

    member __.WithDefaultView () : FieldBuilder =
        { Type = "basic-radio-button"
          State = state
          Name = state.Name
          Config = Radio.config }

    member __.WithCustomView (view) : FieldBuilder =
        { Type = "basic-radio-button"
          State = state
          Name = state.Name
          Config = { Radio.config with View = view } }

    static member Create (name : string) =
        BasicRadio
            { Label = ""
              SelectedKey = None
              Values = []
              Group = (Guid.NewGuid()).ToString()
              Validators = [ ]
              ValidationState = Valid
              Name = name }

    member __.IsRequired (?msg : string) =
        let msg = defaultArg msg "This field is required"

        let validator (state : Radio.State) =
            if Option.isSome state.SelectedKey then
                Valid
            else
                Invalid msg

        BasicRadio { state with Validators = state.Validators @ [ validator ] }

    member __.WithLabel(label : string) =
        BasicRadio { state with Label = label }

    member __.WithValues (values : (Radio.Key * string) list) =
        BasicRadio { state with Values = values }

    member __.WithSelectedKey (key : Radio.Key ) =
        BasicRadio { state with SelectedKey = Some key }

    member __.AddValidator (validator) =
        BasicRadio { state with Validators = state.Validators @ [ validator ] }
