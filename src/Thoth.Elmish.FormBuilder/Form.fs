namespace Thoth.Elmish.FormBuilder

open Elmish
open Fulma
open Fulma.FontAwesome
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import
open Thoth.Json
open System
open Thoth.Elmish
open Fable.PowerPack

module Form =

    type ErrorDef =
        { Text : string
          Key : string }

    type Msg =
        | OnFieldChange of Guid * string
        | DebouncerSelfMsg of Debouncer.SelfMessage<Msg>
        | TriggerServerValidation
        | ServerValidationResult of ErrorDef list
        | OnError of exn
        | SelectInitValues of Guid * (Select.Key * string) list

    [<RequireQualifiedAccess>]
    type Field =
        | Input of Input.InputState
        | Select of Select.SelectState

    type FormState<'AppMsg> =
        { Fields : (Guid * Field) list
          OnFormMsg : (Msg -> 'AppMsg) option
          Actions : Button.ButtonState<'AppMsg> list
          IsWaiting : bool
          Debouncer : Debouncer.State
          ServerValidation : (string -> JS.Promise<ErrorDef list>) option
          GlobalError : string }

    type FormBuilder() =
        member __.Bind(m, f) =
            failwith "bind"

        member __.Return x = failwith "return"

        member __.ReturnFrom() = failwith "return from"

        member __.Zero() = failwith "return from"

        // member __.Combine(formState : FormState<_>, inputState : Input.InputState) =
        //     { formState with Fields = formState.Fields @ [ Field.Input inputState ] }

        member __.For( seq : seq<FormState<_>>, body: 'T -> FormState<_>) =
            Browser.console.log "For #1"
            Browser.console.log seq
            Browser.console.log body
            seq

        member __.For( seq : FormState<_>, body: 'T -> FormState<_>) =
            Browser.console.log "For #2"
            Browser.console.log seq
            Browser.console.log body
            seq

        member __.For( seq : seq<int>, body: Input.InputState -> FormState<_>) : FormState<_> =
            Browser.console.log "For #3"
            Browser.console.log seq
            Browser.console.log body
            unbox null

        member __.Yield(_) : FormState<_> =
            { Fields = [ ]
              OnFormMsg = None
              Actions = [ ]
              IsWaiting = false
              Debouncer = Debouncer.create()
              ServerValidation = None
              GlobalError = "" }

        // member __.YieldFrom( x ) =
        //     printfn "YieldFrom: %A" x
        //     { Fields = [ ]
        //       OnFormMsg = None
        //       Actions = [ ] }

        [<CustomOperation("onChange")>]
        member __.OnChange (formState : FormState<_>, onChange) =
            { formState with OnFormMsg = Some onChange }

        [<CustomOperation("serverValidation")>]
        member __.ServerValidation (formState : FormState<_>, infos) =
            { formState with ServerValidation = Some infos }

        [<CustomOperation("addInput")>]
        member __.Input (formState : FormState<_>, newField) =
            { formState with Fields = formState.Fields @ [ (Guid.NewGuid(), Field.Input newField) ] }

        [<CustomOperation("addSelect")>]
        member __.Select (formState : FormState<_>, newField) =
            { formState with Fields = formState.Fields @ [ (Guid.NewGuid(), Field.Select newField) ] }

        [<CustomOperation("addAction")>]
        member __.addAction (formState : FormState<_>, newAction) =
            { formState with Actions = formState.Actions @ [ newAction ] }

    let init (formState : FormState<'AppMsg>) =
        let cmds =
            formState.Fields
            |> List.map (fun (guid, field) ->
                match field with
                | Field.Input _ ->
                    Cmd.none
                | Field.Select selectState ->
                    match selectState.ValuesFromServer with
                    | Some fetchKeyValues ->
                        let request () =
                            promise {
                                let! keyValues = fetchKeyValues
                                return (guid, keyValues)
                            }
                        Cmd.ofPromise request () SelectInitValues OnError
                    | None -> Cmd.none
            )

        formState, Cmd.batch cmds

    let setWaiting (isWaiting : bool) (formState : FormState<'AppMsg>) =
        { formState with IsWaiting = isWaiting }

    let validate (formState : FormState<'AppMsg>) : FormState<'AppMsg> * bool =
        let newFields =
            formState.Fields
            |> List.map (fun (guid, field) ->
                            let newField =
                                match field with
                                | Field.Input inputState ->
                                    Input.applyValidators inputState
                                    |> Field.Input
                                | Field.Select selectState ->
                                    Select.appyValidators selectState
                                    |> Field.Select

                            (guid, newField)
            )

        let isValid =
            newFields
            |> List.filter (fun (_, field) ->
                match field with
                | Field.Input inputState ->
                    inputState.ValidationInputState <> Valid
                | Field.Select selectState ->
                    selectState.IsLoading || selectState.ValidationSelectState <> Valid
            )
            |> List.length
            |> (=) 0

        { formState with Fields = newFields }, isValid

    let toJson (formState : FormState<'AppMsg>) =
        formState.Fields
        |> List.map (function
            | (_, Field.Input inputState) -> inputState.ToJson()
            | (_, Field.Select selectState) -> selectState.ToJson()
        )
        |> Encode.object
        #if DEBUG
        |> Encode.toString 4
        #else
        |> Encode.toString 0
        #endif

    let render (formState : FormState<'AppMsg>) (dispatch : 'AppMsg -> unit) =
        match formState.OnFormMsg with
        | Some onEvent ->
            let fields =
                let onFieldChange =
                    OnFieldChange >> onEvent >> dispatch

                formState.Fields
                    |> List.map (function
                        | (guid, Field.Input inputState) -> Input.render guid onFieldChange inputState
                        | (guid, Field.Select selectState) -> Select.render guid onFieldChange selectState)

            let actions =
                formState.Actions
                    |> List.map (Button.render dispatch)

            let formClass =
                if formState.IsWaiting then
                    "thoth-form is-waiting"
                else
                    "thoth-form"

            div [ Class formClass ]
                [ yield div [ Class "wait-container" ]
                    [ Icon.faIcon [ ]
                        [ Fa.icon Fa.I.Spinner
                          Fa.fa3x
                          Fa.spin ] ]
                  yield! fields
                  yield Help.help [ Help.Color IsDanger ]
                    [ str formState.GlobalError ]
                  yield Field.div [ Field.IsGroupedCentered ]
                    actions ]

        | None ->
            Message.message [ Message.Color IsWarning ]
                [ Message.body [ ]
                    [ str "You need to provide a `onChange` msg to the form"
                      br [ ]
                      str "Without this message the form can't send event into the application" ] ]

    let update (msg : Msg) (formState : FormState<_>) =
        match msg with
        | OnFieldChange (guid, newValue) ->
            let newFields =
                formState.Fields
                |> List.map (fun (localGuid, field) ->
                    if localGuid <> guid then
                        (localGuid, field)
                    else
                        let newField =
                            match field with
                            | Field.Input inputState ->
                                Input.update inputState newValue
                                |> Field.Input
                            | Field.Select selectState ->
                                Select.update selectState newValue
                                |> Field.Select

                        (localGuid, newField)
                )

            let (debouncerModel, debouncerCmd) =
                formState.Debouncer
                |> Debouncer.bounce (TimeSpan.FromSeconds 1.5) "trigger_server_validation" TriggerServerValidation

            { formState with Fields = newFields
                             Debouncer = debouncerModel }, Cmd.map DebouncerSelfMsg debouncerCmd

        | DebouncerSelfMsg debouncerMsg ->
            let (debouncerModel, debouncerCmd) = Debouncer.update debouncerMsg formState.Debouncer
            { formState with Debouncer = debouncerModel }, debouncerCmd

        | TriggerServerValidation ->
            match formState.ServerValidation with
            | Some request ->
                let json = toJson formState
                formState, Cmd.ofPromise request json ServerValidationResult OnError
            | None -> formState, Cmd.none

        | ServerValidationResult errors ->
            let errors =
                errors
                |> List.groupBy (fun error ->
                    error.Key
                )
                |> Map.ofList

            let newFields =
                formState.Fields
                |> List.map (fun (guid, field) ->
                    match field with
                    | Field.Input inputState ->
                        let newState =
                            match errors.TryGetValue inputState.JsonKey with
                            // We found the key in the errors map, then extract the first error
                            | (true, first::_) ->
                                { inputState with ValidationInputState = Invalid first.Text }
                            | (true, [])
                            | (false, _) -> inputState
                        guid, Field.Input newState
                    | Field.Select selectState ->
                        let newState =
                            match errors.TryGetValue selectState.JsonKey with
                            // We found the key in the errors map, then extract the first error
                            | (true, first::_) ->
                                { selectState with ValidationSelectState = Invalid first.Text }
                            | (true, [])
                            | (false, _) -> selectState

                        guid, Field.Select newState
                )

            let globalError =
                match errors.TryGetValue "global_error" with
                // We found the key in the errors map, then extract the first error
                | (true, first::_) ->
                    first.Text
                | (true, [])
                | (false, _) -> ""

            { formState with Fields = newFields
                             GlobalError = globalError }, Cmd.none

        | SelectInitValues (guid, keyValues) ->
            let newFields =
                formState.Fields
                |> List.map (fun (localGuid, field) ->
                    match localGuid, field with
                    | localGuid, Field.Select selectState when localGuid = guid ->
                        let updatedSelectState =
                            { selectState with Values = keyValues
                                               IsLoading = false }
                        (guid, Field.Select updatedSelectState)
                    | _ ->
                        (localGuid, field)
                )
            { formState with Fields = newFields }, Cmd.none

        | OnError msg ->
            Browser.console.error msg
            formState, Cmd.none
