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

    type InputState =
        { Label : string
          Value : string }

        static member Empty =
            { Label = ""
              Value = "" }

    type Key = string

    type SelectState =
        { Label : string
          Placeholder : (Key * string) option
          SelectedKey : Key option
          Values : (Key * string) list
          IsLoading : bool
          ValuesFromServer : JS.Promise<(Key * string) list> option }

        static member Empty =
            { Label = ""
              Placeholder = None
              SelectedKey = None
              Values = []
              IsLoading = false
              ValuesFromServer = None }

    type IField =
        interface end

    type FieldType =
        | Input
        | Select

    type IFieldMsg =
        interface end

    // type CodeField =
    //     | Input of InputState
    //     | Select of SelectState
    //     interface IField

    type Id = string

    type FieldInfo =
        { Type : FieldType
          State : obj
          Id : Id }

    type Msg =
        | DebouncerSelfMsg of Debouncer.SelfMessage<Msg>
        | OnFieldMessage of Id * obj

    type Form<'AppMsg> =
        { Fields : FieldInfo list
          OnFormMsg : Msg -> 'AppMsg }

    type FieldConfig =
        { Render : obj -> (IFieldMsg -> unit) -> React.ReactElement
          Update : obj -> obj -> obj * (Id -> Cmd<Msg>)
          Init : obj -> obj * (Id -> Cmd<Msg>) }

    type Config = Map<FieldType, FieldConfig>

    // let render (form : Form) =
    //     let fields =
    //         form.Fields
    //         |> List.map (fun field ->

    //         )

    module Form =
        module Cmd =
            let none (_id : Id) : Cmd<Msg> =
                [ ]

            let ofPromise (task: 'a -> Fable.Import.JS.Promise<'FieldMessage>)
                          (arg:'a)
                          (fieldId : Id) : Cmd<Msg> =

                let bind dispatch =
                    task arg
                    |> Promise.map (fun fieldMessage ->
                        OnFieldMessage (fieldId, fieldMessage)
                        |> dispatch
                    )
                    |> Promise.catch (fun error ->
                        OnFieldMessage (fieldId, error)
                        |> dispatch
                    )
                    |> ignore

                [ bind ]

    module Input =

        type Msg =
            | ChangeValue of string
            interface IFieldMsg

        let init (state : obj) =
            state, Form.Cmd.none

        let update (state : obj) (msg : obj) =
            let msg = msg :?> Msg
            let state = state :?> InputState

            match msg with
            | ChangeValue newValue ->
                box { state with Value = newValue }, Form.Cmd.none

        let render (state : obj) (onChange : IFieldMsg -> unit) =
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

    module Select =

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

        let init (state : obj) =
            let state = state :?> SelectState
            let cmd =
                match state.ValuesFromServer with
                | Some fetchKeyValues ->
                    let request () =
                        promise {
                            let! keyValues = fetchKeyValues
                            return ReceivedValueFromServer keyValues
                        }

                    Form.Cmd.ofPromise request ()

                | None -> Form.Cmd.none

            box { state with IsLoading = true }, cmd

        let update (state : obj) (msg : obj) =
            let msg = msg :?> Msg
            let state = state :?> SelectState

            match msg with
            | ChangeValue selectedKey ->
                box { state with SelectedKey = Some selectedKey }, Form.Cmd.none

            | ReceivedValueFromServer values ->
                box { state with IsLoading = false
                                 Values = values }, Form.Cmd.none

        let render (state : obj) (onChange : IFieldMsg -> unit) =
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

    let create (onFormMsg : Msg -> 'AppMsg) : Form<'AppMsg> =
        { Fields = []
          OnFormMsg = onFormMsg }

    let addField field form =
        { form with Fields = form.Fields @ [ field ] }


    let config =
        Map.empty<FieldType, FieldConfig>
        |> Map.add Input
            { Render = Input.render
              Update = Input.update
              Init = Input.init }
        |> Map.add Select
            { Render = Select.render
              Update = Select.update
              Init = Select.init }

    let init (form : Form<_>) =
        let mappedFields =
            form.Fields
            |> List.map (fun info ->
                match Map.tryFind info.Type config with
                | Some config ->
                    let newState, cmd = config.Init info.State
                    { info with State = newState }, cmd info.Id
                | None -> failwithf "Seems like you didn't register `FieldType.%s`" (string info.Type)
            )

        let fields = mappedFields |> List.map fst
        let cmds = mappedFields |> List.map snd

        { form with Fields = fields }, Cmd.batch cmds

    let update (msg : Msg) (form : Form<_>) =
        match msg with
        | OnFieldMessage (fieldId, msg) ->
            let mappedFields =
                form.Fields
                |> List.map (fun info ->
                    if info.Id = fieldId then
                        match Map.tryFind info.Type config with
                        | Some config ->
                            let newState, cmd = config.Update info.State msg
                            { info with State = newState }, cmd info.Id
                        | None -> failwithf "Seems like you didn't register `FieldType.%s`" (string info.Type)
                    else
                        info, Form.Cmd.none info.Id
                )

            let fields = mappedFields |> List.map fst
            let cmds = mappedFields |> List.map snd

            { form with Fields = fields }, Cmd.batch cmds

    let render (form : Form<_>) dispatch =
        form.Fields
        |> List.map (fun info ->
            match Map.tryFind info.Type config with
            | Some config ->
                let onFieldChange guid =
                    (fun v -> OnFieldMessage (guid, v)) >> form.OnFormMsg >> dispatch
                fragment [ FragmentProp.Key info.Id ]
                    [ config.Render info.State (onFieldChange info.Id) ]
            | None ->
                Browser.console.error (sprintf "Seems like you didn't register `FieldType.%s`" (string info.Type))
                str "Faill"
        )
        |> ofList
