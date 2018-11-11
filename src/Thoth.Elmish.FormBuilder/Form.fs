namespace Thoth.Elmish.FormBuilder

open Elmish
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Types
open System
open Thoth.Json

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
        let unkownFields =
            form.Fields
            |> List.filter (fun info ->
                not (Map.containsKey info.Type config)
            )

        if unkownFields.Length <> 0 then
            let mutable msg = "You didn't register the following types in your config:"

            for field in unkownFields do
                msg <- msg + "\n" + "- " + field.Type
            failwith msg

        let mappedFields =
            form.Fields
            |> List.map (fun info ->
                let fieldConfig = Map.find info.Type config

                let newState, cmd = fieldConfig.Init info.State
                { info with State = newState }, cmd info.Guid

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
                        let fieldConfig = Map.find info.Type config

                        let newState, cmd = fieldConfig.Update msg info.State
                        { info with State = newState }, cmd info.Guid
                    else
                        info, FormCmd.none info.Guid
                )

            let fields = mappedFields |> List.map fst
            let cmds = mappedFields |> List.map snd

            { form with Fields = fields }, Cmd.batch cmds

    let render (config : Config) (form : Form<_>) dispatch =
        form.Fields
        |> List.map (fun info ->
            let fieldConfig = Map.find info.Type config

            let onFieldChange guid =
                (fun v -> OnFieldMessage (guid, v)) >> form.OnFormMsg >> dispatch

            fragment [ FragmentProp.Key (info.Guid.ToString()) ]
                [ fieldConfig.Render info.State (onFieldChange info.Guid) ]
        )
        |> ofList

    let validate (config : Config) (form : Form<'AppMsg>) : Form<'AppMsg> * bool =
        let newFields =
            form.Fields
            |> List.map (fun field ->
                let fieldConfig = Map.find field.Type config
                { field with State = fieldConfig.Validate field.State }
            )

        let isValid =
            newFields
            |> List.filter (fun field ->
                let fieldConfig = Map.find field.Type config
                fieldConfig.IsValid field.State
            )
            |> List.length
            |> (=) 0

        { form with Fields = newFields }, isValid

    let toJson (config : Config) (form : Form<'AppMsg>) : string =
        form.Fields
        |> List.map (fun field ->
            let fieldConfig = Map.find field.Type config
            fieldConfig.ToJson field.State
        )
        |> Encode.object
        #if DEBUG
        |> Encode.toString 4
        #else
        |> Encode.toString 0
        #endif
