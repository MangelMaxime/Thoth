namespace Thoth.Elmish.FormBuilder

open Fulma
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import
open Thoth.Json
open System

module Select =

    type Key = string

    type SelectState =
        { Label : string
          JsonLabel : string option
          SelectedKey : Key option
          Values : (Key * string) list
          Placeholder : (Key * string) option
          Validators : SelectValidator list
          ValidationSelectState : ValidationState
          IsLoading : bool
          ValuesFromServer : JS.Promise<(Key * string) list> option }

        member this.JsonKey
            with get () = this.JsonLabel
                            |> Option.defaultValue this.Label

        member this.ToJson () =
            let value =
                match this.SelectedKey with
                | Some key -> Encode.string key
                | None -> Encode.nil

            this.JsonKey, value

    and SelectValidator = SelectState -> ValidationState

    type SelectBuilder() =

        member __.Yield(_) : SelectState =
            { Label = ""
              SelectedKey = None
              JsonLabel = None
              Values = []
              Validators = []
              Placeholder = None
              ValidationSelectState = Valid
              IsLoading = true
              ValuesFromServer = None }

        [<CustomOperation("label")>]
        member __.Label (selectState : SelectState, label : string) =
            { selectState with Label = label }

        [<CustomOperation("jsonLabel")>]
        member __.JsonLabel (selectState : SelectState, jsonLabel : string) =
            { selectState with JsonLabel = Some jsonLabel }

        [<CustomOperation("values")>]
        member __.Values (selectState : SelectState, values : (Key * string) list) =
            { selectState with Values = values
                               IsLoading = false }

        [<CustomOperation("valuesFromServer")>]
        member __.ValuesFromServer (selectState : SelectState, valuesFromServer : JS.Promise<(Key * string) list>) =
            { selectState with ValuesFromServer = Some valuesFromServer
                               IsLoading = true }

        [<CustomOperation("isRequired")>]
        member __.IsRequired (selectState : SelectState) =
            let apply (selectState : SelectState) =
                if selectState.SelectedKey.IsNone then
                    Invalid "This field is required"
                else
                    Valid

            { selectState with Validators = selectState.Validators @ [apply] }

        [<CustomOperation("placeholder")>]
        member __.Placeholder (selectState : SelectState, key, placeholder) =
            { selectState with Placeholder = Some (key, placeholder) }

    let internal appyValidators (selectState : SelectState) =
        let rec apply (validators : SelectValidator list) (selectState : SelectState) =
            match validators with
            | validator::rest ->
                match validator selectState with
                | Valid -> apply rest selectState
                | Invalid msg ->
                    { selectState with ValidationSelectState = Invalid msg }
            | [] -> selectState

        apply selectState.Validators selectState

    let update (inputState : SelectState) (newValue : Key) =
        { inputState with SelectedKey = Some newValue
                          ValidationSelectState = Valid }
        |> appyValidators

    let private renderOption (key,value) =
        option [ Value key ]
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

    let render onChange (selectState : SelectState) =
        Field.div [ ]
            [ Label.label [ ]
                [ str selectState.Label ]
              Control.div [ ]
                [ Select.select [ Select.IsLoading selectState.IsLoading
                                  Select.IsFullWidth ]
                    [ select [ Value (selectState.SelectedKey |> Option.defaultValue "")
                               OnChange (fun ev ->
                                    ev.Value |> onChange
                                ) ]
                        [ yield renderPlaceHolder selectState.Placeholder
                          yield! (List.map renderOption selectState.Values) ] ] ]
              Help.help [ Help.Color IsDanger ]
                [ str selectState.ValidationSelectState.ToText ] ]
