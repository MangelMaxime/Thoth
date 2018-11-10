namespace Thoth.Elmish.FormBuilder

open Elmish
open Fulma
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import
open Fable.PowerPack
open Types

module FormCmd = Thoth.Elmish.FormBuilder.Cmd

[<RequireQualifiedAccess>]
module Form =

    let create (onFormMsg : Msg -> 'AppMsg) : Form<'AppMsg> =
        { Fields = []
          OnFormMsg = onFormMsg }

    let addField field form =
        { form with Fields = form.Fields @ [ field ] }

    let init (config : Config) (form : Form<_>) =
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

    let update (config : Config) (msg : Msg) (form : Form<_>) =
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
                        info, FormCmd.none info.Id
                )

            let fields = mappedFields |> List.map fst
            let cmds = mappedFields |> List.map snd

            { form with Fields = fields }, Cmd.batch cmds

    let render (config : Config) (form : Form<_>) dispatch =
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
