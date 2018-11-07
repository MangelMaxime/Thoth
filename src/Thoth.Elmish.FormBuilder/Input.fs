namespace Thoth.Elmish.FormBuilder

open Fulma
open Fable.Helpers.React
open Thoth.Json
open System

module Input =

    type InputState =
        { Label : string
          JsonLabel : string option
          Type : string
          Value : string
          Placeholder : string option
          Validators : InputValidator list
          ValidationInputState : ValidationState }

        member this.JsonKey
            with get () = this.JsonLabel
                            |> Option.defaultValue this.Label

        member this.ToJson () =
            this.JsonKey, Encode.string this.Value

    and InputValidator = InputState -> ValidationState

    type InputBuilder() =

        member __.Yield(_) : InputState =
            { Label = ""
              JsonLabel = None
              Type = "text"
              Value = ""
              Validators = []
              Placeholder = None
              ValidationInputState = Valid }

        [<CustomOperation("label")>]
        member __.Label (inputState : InputState, label : string) =
            { inputState with Label = label }

        [<CustomOperation("jsonLabel")>]
        member __.JsonLabel (inputState : InputState, jsonLabel : string) =
            { inputState with JsonLabel = Some jsonLabel }

        [<CustomOperation("typ")>]
        member __.Type (inputState : InputState, label : string) =
            { inputState with Label = label }

        [<CustomOperation("isRequired")>]
        member __.IsRequired (inputState : InputState) =
            let apply (inputState : InputState) =
                if inputState.Value = "" then
                    Invalid "This field is required"
                else
                    Valid

            { inputState with Validators = inputState.Validators @ [apply] }

        [<CustomOperation("placeholder")>]
        member __.Placeholder (inputState : InputState, placeholder) =
            { inputState with Placeholder = Some placeholder }

    let internal applyValidators (inputState : InputState) =
        let rec apply (validators : InputValidator list) (inputState : InputState) =
            match validators with
            | validator::rest ->
                match validator inputState with
                | Valid -> apply rest inputState
                | Invalid msg ->
                    { inputState with ValidationInputState= Invalid msg }
            | [] -> inputState

        apply inputState.Validators inputState

    let update (inputState : InputState) (newValue : string) =
        { inputState with Value = newValue
                          ValidationInputState= Valid }
        |> applyValidators

    let render onChange (inputState : InputState) =
        Field.div [ ]
            [ Label.label [ ]
                [ str inputState.Label ]
              Control.div [ ]
                [ Input.input [ Input.Value inputState.Value
                                Input.Placeholder (inputState.Placeholder |> Option.defaultValue "")
                                Input.OnChange (fun ev ->
                                    ev.Value |> onChange
                                ) ] ]
              Help.help [ Help.Color IsDanger ]
                [ str inputState.ValidationInputState.ToText ] ]
