module Demos.FormBuilder

open Elmish
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Thoth.Elmish
open System
open Thoth.Elmish.FormBuilder
open Thoth.Elmish.FormBuilder.BasicFields
open Fable.PowerPack
open Thoth.Json
open Fable.Import

module FakeServer =

    let decodeIntFromString =
        Decode.string
        |> Decode.map int

    let decoderAllowPublic : Decode.Decoder<bool> =
        decodeIntFromString
        |> Decode.andThen (
            function
            | 1 ->
                Decode.succeed true

            | 2 ->
                Decode.succeed false

            | unkown ->
                sprintf "`%i` isn't a valid representation for publicSetting" unkown
                |> Decode.fail
        )

    type FormData =
        { Name : string
          FavLang : int
          Description : string
          Condition : bool
          AllowPublic : bool }

        static member Decoder =
            Decode.object (fun get ->
                { Name = get.Required.Field "name" Decode.string
                  FavLang = get.Required.Field "favLang" decodeIntFromString
                  Description = get.Required.Field "description" Decode.string
                  Condition = get.Required.Field "condition" Decode.bool
                  AllowPublic = get.Required.Field "publicSetting" decoderAllowPublic } : FormData
            )

    let getLanguages () =
        promise {
            do! Promise.sleep 2000
            return [
                "1", "C"
                "10", "C#"
                "4", "Clojure"
                "7", "Elm"
                "9", "F#"
                "269", "JavaScript"
            ]
        }

    let createProfile (body : string) =
        promise {
            do! Promise.sleep 1000
            match Decode.fromString FormData.Decoder body with
            | Ok formData ->
                let errors =
                    [ if formData.Name.ToLower() = "Test".ToLower() then
                        let msg = sprintf """A "%s" profile already exist""" formData.Name
                        yield ({ Key = "name"
                                 Text = msg } : Types.ErrorDef)
                    ]

                let result =
                    if errors.Length = 0 then
                        Encode.object [
                            "code", Encode.string "ok"
                        ]
                    else
                        Encode.object [
                            "code", Encode.string "errors"
                            "data", errors
                                    |> List.map Types.ErrorDef.Encoder
                                    |> Encode.list
                        ]

                return Encode.toString 4 result

            | Error msg ->
                return failwith msg
        }

type CreationResponse =
    | Ok
    | Errors of Types.ErrorDef list

type Msg =
    | Submit
    | OnFormMsg of FormBuilder.Types.Msg
    | DebouncerSelfMsg of Debouncer.SelfMessage<Msg>
    | CreateResponse of CreationResponse
    | OnError of exn
    | Reset

type FormSate = FormBuilder.Types.State

type State =
    | Completed
    | Editing of FormSate

type Model =
    { State : State
      Debouncer : Debouncer.State }

let getLanguages =
    promise {
        let! res = FakeServer.getLanguages ()
        return res
    }

let createProfile (body : string) : JS.Promise<CreationResponse> =
    promise {
        let! res = FakeServer.createProfile body
        let decoder =
            Decode.field "code" Decode.string
            |> Decode.andThen (
                function
                | "ok" ->
                    Decode.succeed CreationResponse.Ok
                | "errors" ->
                    Decode.field "data" (Decode.list Types.ErrorDef.Decoder)
                    |> Decode.map CreationResponse.Errors
                | unkown ->
                    sprintf "`%s` is an unkown code" unkown
                    |> Decode.fail
            )

        let result =
            match Decode.fromString decoder res with
            | Result.Ok result -> result
            | Error msg -> failwith msg

        return result
    }

let formState, formConfig =
    Form<Msg>
        .Create(OnFormMsg)
        .AddField(
            BasicInput
                .Create("name")
                .WithLabel("Name")
                .IsRequired()
                .WithDefaultView()
        )
        .AddField(
            BasicSelect
                .Create("favLang")
                .WithLabel("Favorite language")
                .WithValuesFromServer(getLanguages)
                .WithPlaceholder("")
                .IsRequired("I know it's hard but you need to choose")
                .WithDefaultView()
        )
        .AddField(
            BasicTextarea
                .Create("description")
                .WithLabel("Description")
                .IsRequired()
                .WithPlaceholder("Here you can introduce yourself...")
                .AddValidator(fun state ->
                    if state.Value.Length < 10 then
                        Types.Invalid "You need to enter a description of at least 10 characters"
                    else
                        Types.Valid
                )
                .WithDefaultView()
        )
        .AddField(
            BasicCheckbox
                .Create("condition")
                .WithLabel("I agree with the terms and conditions")
                .IsRequired()
                .WithDefaultView()
        )
        .AddField(
            BasicRadio
                .Create("publicSetting")
                .WithLabel("Make your profile public ?")
                .IsRequired()
                .WithValues([
                    "1", "Yes"
                    "2", "No"
                ])
                .WithDefaultView()
        )
        .Build()

let private init _ =
    let (formState, formCmds) = Form.init formConfig formState

    { State = Editing formState
      Debouncer = Debouncer.create () }, Cmd.map OnFormMsg formCmds

let inline applyIfEditing (model : Model) (f : FormSate -> Model * Cmd<Msg>) =
    match model.State with
    | Editing formState ->
        f formState
    | Completed _ ->
        model, Cmd.none

let private update msg model =
    match msg with
    | OnFormMsg msg ->
        applyIfEditing
            model
            (fun formState ->
                let (newFormState, formCmd) = Form.update formConfig msg formState
                { model with State = Editing newFormState }, Cmd.map OnFormMsg formCmd
            )

    | Submit ->
        applyIfEditing
            model
            (fun formState ->
                let (newFormState, isValid) = Form.validate formConfig formState
                if isValid then
                    let body =
                        Form.toJson formConfig newFormState

                    { model with State = Editing (Form.setLoading true newFormState) }, Cmd.ofPromise createProfile body CreateResponse OnError
                else
                    { model with State = Editing newFormState }, Cmd.none
            )

    | CreateResponse response ->
        applyIfEditing
            model
            (fun formState ->
                match response with
                | CreationResponse.Ok ->
                    let (debouncerModel, debouncerCmd) =
                        model.Debouncer
                        |> Debouncer.bounce (TimeSpan.FromSeconds 3.0) "form_reset" Reset
                    { model with State = Completed
                                 Debouncer = debouncerModel }, Cmd.map DebouncerSelfMsg debouncerCmd
                | CreationResponse.Errors errors ->
                    let newFormState =
                        formState
                        |> Form.setLoading false
                        |> Form.setErrors formConfig errors
                    { model with State = Editing newFormState }, Cmd.none
            )

    | OnError error ->
        Browser.console.error error
        applyIfEditing
            model
            (fun formState ->
                { model with State = Editing (Form.setLoading false formState) }, Cmd.none
            )

    | Reset ->
        init ()

    | DebouncerSelfMsg debouncerMsg ->
        let (debouncerModel, debouncerCmd) = Debouncer.update debouncerMsg model.Debouncer
        { model with Debouncer = debouncerModel }, debouncerCmd


open Fulma

let private formActions (formState : FormSate) dispatch =
    Field.div [ Field.IsGrouped
                Field.IsGroupedCentered ]
        [ Control.div [ ]
            [ Button.button [ Button.Color IsPrimary
                              Button.IsLoading (Form.isLoading formState)
                              Button.OnClick (fun _ ->
                                dispatch Submit
                              ) ]
                [ str "Submit" ] ] ]

let private viewFormEditing (formState : FormSate) dispatch =
    Form.render
        { Config = formConfig
          State = formState
          Dispatch = dispatch
          ActionsArea = (formActions formState dispatch)
          Loader = Form.DefaultLoader }

let private view (model : Model) dispatch =
    let content =
        match model.State with
        | Editing formState ->
            div [ ]
                [ Message.message [ Message.Color IsInfo ]
                    [ Message.body [ ]
                        [ str "If you want to test the server side validation feature, enter \"Test\" in the \"Name\" field." ] ]
                  viewFormEditing formState dispatch ]
        | Completed ->
            Message.message [ Message.Color IsInfo ]
                [ Message.header [ ]
                    [ str "Your profile has been created" ]
                  Message.body [ ]
                    [ str "The demo will reset in a few seconds" ] ]

    div [ Style [ MaxWidth "500px"
                  MinHeight "530px"
                  PaddingTop "10px" ] ]
        [ content ]

open Elmish.React
open Elmish.Debug
open Elmish.HMR

let start (id : string) =
    Program.mkProgram init update view
    |> Program.withReactUnoptimized id
    |> Program.run
