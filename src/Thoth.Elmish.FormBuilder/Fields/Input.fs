namespace Thoth.Elmish.FormBuilder.Fields

open Fulma
open Fable.Helpers.React
open Thoth.Elmish

module FormCmd = Thoth.Elmish.FormBuilder.Cmd

[<RequireQualifiedAccess>]
module Input =

    type InputState =
        { Label : string
          Value : string }

    type Msg =
        | ChangeValue of string
        interface FormBuilder.Types.IFieldMsg

    let init (state : FormBuilder.Types.FieldState) =
        state, FormCmd.none

    let update (msg : FormBuilder.Types.FieldMsg) (state : FormBuilder.Types.FieldState) =
        let msg = msg :?> Msg
        let state = state :?> InputState

        match msg with
        | ChangeValue newValue ->
            box { state with Value = newValue }, FormCmd.none

    let render (state : FormBuilder.Types.FieldState) (onChange : FormBuilder.Types.IFieldMsg -> unit) =
        let state : InputState = state :?> InputState
        Field.div [ ]
            [ Label.label [ ]
                [ str state.Label ]
              Control.div [ ]
                [ Input.input [ Input.Value state.Value
                                // Input.Placeholder (state.Placeholder |> Option.defaultValue "")
                                Input.OnChange (fun ev ->
                                    ev.Value |> ChangeValue |> onChange
                                ) ] ]
            //   Help.help [ Help.Color IsDanger ]
            //     [ str state.ValidationInputState.ToText ]
                ]

    let create (label : string) : InputState =
        { Label = ""
          Value = "" }

    let withValue (value : string) (state : InputState) =
        { state with Value = value }

    // type InputBuilder() =

    //     member __.Yield(_) : InputState =
    //         { Label = ""
    //           JsonLabel = None
    //           Type = "text"
    //           Value = ""
    //           Validators = []
    //           Placeholder = None
    //           ValidationInputState = Valid }

    //     [<CustomOperation("label")>]
    //     member __.Label (inputState : InputState, label : string) =
    //         { inputState with Label = label }

    //     [<CustomOperation("jsonLabel")>]
    //     member __.JsonLabel (inputState : InputState, jsonLabel : string) =
    //         { inputState with JsonLabel = Some jsonLabel }

    //     [<CustomOperation("typ")>]
    //     member __.Type (inputState : InputState, label : string) =
    //         { inputState with Label = label }

    //     [<CustomOperation("isRequired")>]
    //     member __.IsRequired (inputState : InputState) =
    //         let apply (inputState : InputState) =
    //             if inputState.Value = "" then
    //                 Invalid "This field is required"
    //             else
    //                 Valid

    //         { inputState with Validators = inputState.Validators @ [apply] }

    //     [<CustomOperation("placeholder")>]
    //     member __.Placeholder (inputState : InputState, placeholder) =
    //         { inputState with Placeholder = Some placeholder }

    // let internal applyValidators (inputState : InputState) =
    //     let rec apply (validators : InputValidator list) (inputState : InputState) =
    //         match validators with
    //         | validator::rest ->
    //             match validator inputState with
    //             | Valid -> apply rest inputState
    //             | Invalid msg ->
    //                 { inputState with ValidationInputState= Invalid msg }
    //         | [] -> inputState

    //     apply inputState.Validators inputState

    // let update (inputState : InputState) (newValue : string) =
    //     { inputState with Value = newValue
    //                       ValidationInputState= Valid }
    //     |> applyValidators

    // let render onChange (inputState : InputState) =
    //     Field.div [ ]
    //         [ Label.label [ ]
    //             [ str inputState.Label ]
    //           Control.div [ ]
    //             [ Input.input [ Input.Value inputState.Value
    //                             Input.Placeholder (inputState.Placeholder |> Option.defaultValue "")
    //                             Input.OnChange (fun ev ->
    //                                 ev.Value |> onChange
    //                             ) ] ]
    //           Help.help [ Help.Color IsDanger ]
    //             [ str inputState.ValidationInputState.ToText ] ]
