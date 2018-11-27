namespace Thoth.Elmish.FormBuilder.BasicFields

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Thoth.Elmish.FormBuilder
open Thoth.Elmish.FormBuilder.BasicFields
open Thoth.Elmish.FormBuilder.Types
open System
open Thoth.Json

[<RequireQualifiedAccess>]
module Input =

    type State =
        { Label : string
          Value : string
          Type : string
          Placeholder : string option
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
        state.Name, Encode.string state.Value

    let private update (msg : FieldMsg) (state : FieldState) =
        let msg = msg :?> Msg
        let state = state :?> State

        match msg with
        | ChangeValue newValue ->
            { state with Value = newValue }
            |> validate
            |> box, FormCmd.none

    let private view (state : FieldState) (dispatch : IFieldMsg -> unit) =
        let state : State = state :?> State
        let className =
            if isValid state then
                "input"
            else
                "input is-danger"

        div [ Class "field" ]
            [ label [ Class "label"
                      HtmlFor state.Name ]
                [ str state.Label ]
              div [ Class "control" ]
                [ input [ Value state.Value
                          Placeholder (state.Placeholder |> Option.defaultValue "")
                          Id state.Name
                          Class className
                          HTMLAttr.Type state.Type
                          OnChange (fun ev ->
                            ChangeValue ev.Value |> dispatch
                          ) ] ]
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

type BasicInput private (state : Input.State) =

    member __.WithDefaultView () : FieldBuilder =
        { Type = "basic-input"
          State = state
          Name = state.Name
          Config = Input.config }

    member __.WithCustomView (view) : FieldBuilder =
        { Type = "basic-input"
          State = state
          Name = state.Name
          Config = { Input.config with View = view } }

    static member Create(name : string) =
        BasicInput
            { Label = ""
              Value = ""
              Type = "text"
              Placeholder = None
              Validators = [ ]
              ValidationState = Valid
              Name = name }

    member __.WithLabel (label : string) =
        BasicInput { state with Label = label }

    member __.WithValue (value : string) =
        BasicInput { state with Value = value }

    member __.WithType (typ : string) =
        BasicInput { state with Type = typ }

    member __.WithPlaceholder (placeholder : string) =
        BasicInput { state with Placeholder = Some placeholder }

    member __.IsRequired (?msg : String) =
        let msg = defaultArg msg "This field is required"

        let validator (state : Input.State) =
            if String.IsNullOrWhiteSpace state.Value then
                Invalid msg
            else
                Valid

        BasicInput { state with Validators = state.Validators @ [ validator ] }

    member __.AddValidator (validator) =
        BasicInput { state with Validators = state.Validators @ [ validator ] }
