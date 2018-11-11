namespace Thoth.Elmish.FormBuilder

open Elmish
open Fulma
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import
open Fable.PowerPack
open Types
open System

module FormCmd = Thoth.Elmish.FormBuilder.Cmd

[<RequireQualifiedAccess>]
module Form =

    let create (onFormMsg : Msg -> 'AppMsg) : Form<'AppMsg> =
        { Fields = []
          OnFormMsg = onFormMsg }

    let addField (field : Field) (form : Form<_>) =
        // Make sure that the field has a unique GUID
        // The chance of duplication are really small but if we had a duplicate it would break
        // the whole form state so better to check
        let rec enforceUniqueId (form : Form<_>) (field : Field)  =
            form.Fields
            |> List.exists (fun localField ->
                field.Guid = localField.Guid
            )
            |> function
            | true ->
                enforceUniqueId form { field with Guid = Guid.NewGuid() }
            | false -> field

        let securedField = enforceUniqueId form field

        { form with Fields = form.Fields @ [ securedField ] }

    let init (config : Config) (form : Form<_>) =
        let mappedFields =
            form.Fields
            |> List.map (fun info ->
                match Map.tryFind info.Type config with
                | Some config ->
                    let newState, cmd = config.Init info.State
                    { info with State = newState }, cmd info.Guid
                | None -> failwithf "Seems like you didn't register FieldType: `%s`" (string info.Type)
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
                    if info.Guid = fieldId then
                        match Map.tryFind info.Type config with
                        | Some config ->
                            let newState, cmd = config.Update msg info.State
                            { info with State = newState }, cmd info.Guid
                        | None -> failwithf "Seems like you didn't register FieldType: `%s`" (string info.Type)
                    else
                        info, FormCmd.none info.Guid
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
                fragment [ FragmentProp.Key (info.Guid.ToString()) ]
                    [ config.Render info.State (onFieldChange info.Guid) ]
            | None ->
                Browser.console.error (sprintf "Seems like you didn't register FieldType: `%s`" (string info.Type))
                str "Fail"
        )
        |> ofList
