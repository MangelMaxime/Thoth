namespace Thoth.Elmish.FormBuilder

open Elmish
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Types
open System
open Thoth.Json
open Fable.Import

type Form<'AppMsg> private (onFormMsg : Msg -> 'AppMsg, fieldBuilders : Types.FieldBuilder list) =

    static member Create(onFormMsg) =
        Form (onFormMsg, [])

    member __.AddField(fieldBuilder : FieldBuilder) =
        Form (onFormMsg, fieldBuilders @ [ fieldBuilder ] )

    member __.Build() =
        let duplicatesName =
            fieldBuilders
            |> List.groupBy (fun builder -> builder.Name)
            |> List.filter (fun (_, set) -> set.Length > 1)
            |> List.map fst

        if duplicatesName.Length > 0 then
            let mutable msg = "Each field needs to have a unique name. I found the following duplicates name:"

            for name in duplicatesName do
                msg <- msg + "\n-" + name

            failwith msg

        let config : Types.Config<'AppMsg> =
            { ToMsg = onFormMsg
              FieldsConfig =
                fieldBuilders
                |> List.map (fun builder ->
                    builder.Type, builder.Config
                )
                |> Map.ofList
            }

        let fields : Types.Field list =
            fieldBuilders
            |> List.map (fun builder ->
                { Type = builder.Type
                  State = builder.State
                  Name = builder.Name }
            )

        { Fields = fields
          IsLoading = false }, config

[<RequireQualifiedAccess>]
module Form =

    module private Styles =

        let inline form<'a> =
            [ Position "relative" ]
            |> Style

        let inline loaderContainer<'a> =
            [ Display "flex"
              Position "absolute"
              JustifyContent "center"
              Width "100%"
              Height "100%"
              AlignItems "center"
              ZIndex 10
              Opacity 0.4
              BackgroundColor "white" ]
            |> Style

        // This code has been adapted from
        // https://loading.io/css/
        let inline loaderRings<'a> =
            [ Content " "
              Display "block"
              Width "46px"
              Height "46px"
              Margin "1px"
              BorderRadius "50%"
              Border "5px solid #000"
              BorderColor "#000 transparent #000 transparent"
              Animation "thoth-form-loader-rings 1.2s linear infinite" ]
            |> Style

        let [<Literal>] loaderRingsFramesCSS =
            """@keyframes thoth-form-loader-rings {
  0% {
    transform: rotate(0deg);
  }
  100% {
    transform: rotate(360deg);
  }
}"""

    type LoaderContainer =
        | DefaultLoader
        | CustomLoader of (bool -> React.ReactElement)

    type FormRenderProps<'Msg> =
        { Config : Config<'Msg>
          State : State
          Dispatch : 'Msg -> unit
          ActionsArea : React.ReactElement
          Loader : LoaderContainer }

    /// `init` function to call from your `init` to initialize the form
    /// Returns:
    /// - `formState` - `State` - New state to be stored in your Model
    /// - `formCmd` - `Cmd<Msg>` - Commands to be mapped in your Elmish application
    let init (config : Config<_>) (form : State) =
        let mappedFields =
            form.Fields
            |> List.map (fun info ->
                let fieldConfig = Map.find info.Type config.FieldsConfig

                let newState, cmd = fieldConfig.Init info.State
                { info with State = newState }, cmd info.Name
            )

        let fields = mappedFields |> List.map fst
        let cmds = mappedFields |> List.map snd

        { form with Fields = fields }, Cmd.batch cmds

    /// `update` function to call when you received a message for the form
    /// Returns:
    /// - `formState` - `State` - New state to be stored in your Model
    /// - `formCmd` - `Cmd<Msg>` - Commands to be mapped in your Elmish application
    let update (config : Config<_>) (msg : Msg) (form : State) =
        match msg with
        | OnFieldMessage (fieldName, msg) ->
            let mappedFields =
                form.Fields
                |> List.map (fun info ->
                    if info.Name = fieldName then
                        let fieldConfig = Map.find info.Type config.FieldsConfig

                        let newState, cmd = fieldConfig.Update msg info.State
                        { info with State = newState }, cmd info.Name
                    else
                        info, FormCmd.none info.Name
                )

            let fields = mappedFields |> List.map fst
            let cmds = mappedFields |> List.map snd

            { form with Fields = fields }, Cmd.batch cmds

    let inline private defaultLoader (isLoading : bool) =
        if isLoading then
            div [ Class "loader-container"
                  Styles.loaderContainer
                  // When using the defaultLoader, we append the needs key frames
                  // by appending a manually created `style` element
                  // We do that, in order to make Thoth.Elmish.FormBuilder
                  // directly usable without asking the user to load any CSS styles
                  // Also, because all the functions/styles relative to the default loader
                  // are marked as `inline`, they will not be included in the bundle is not used
                  Ref (fun elt ->
                    if not (isNull elt) then
                        Helpers.appendStyle
                            "thoth-form-loader-container-key-frames"
                            Styles.loaderRingsFramesCSS
                  ) ]
                [ div [ Class "loader-rings"
                        Styles.loaderRings ]
                    [ ] ]
        else
            nothing

    /// Render the form in your view
    /// Props description:
    /// - `Config` - `Config` - Configuration of your form. You get it when calling `From.init`
    /// - `State` - `State<'Msg>` - Current state of the form, it's coming from your `Model`
    /// - `Dispatch` - `'Msg -> unit` - The `dispatch` coming from Elmish
    /// - `ActionsArea` - `React.ReactElement` - A `ReactElement` to render at the bottom of the form. In general, it contains your buttons.
    /// - `Loader` - `LoaderContainer` - Either `Form.DefaultLoader` or `Form.CustomLoader`. This represents the `ReactElement` that will been display when the format is marked as loading
    let render (props : FormRenderProps<'Msg>) =
        let fields =
            props.State.Fields
            |> List.map (fun info ->
                let fieldConfig = Map.find info.Type props.Config.FieldsConfig

                let onFieldChange guid =
                    (fun v -> OnFieldMessage (guid, v)) >> props.Config.ToMsg >> props.Dispatch

                fragment [ FragmentProp.Key info.Name ]
                    [ fieldConfig.View info.State (onFieldChange info.Name) ]
            )
            |> ofList

        let loader =
            match props.Loader with
            | DefaultLoader ->
                defaultLoader props.State.IsLoading
            | CustomLoader loader ->
                loader props.State.IsLoading

        div [ Class "thoth-form"
              Styles.form ]
            [ loader
              fields
              props.ActionsArea ]

    /// Validate the model and check if it's valid
    /// Returns a tuple of 2 elements
    /// - First element is the new state of the form to store in your model
    /// - Second element is `true`, if the form is valid. `false` otherwise
    let validate (config : Config<_>) (form : State) : State * bool =
        let newFields =
            form.Fields
            |> List.map (fun field ->
                let fieldConfig = Map.find field.Type config.FieldsConfig
                { field with State = fieldConfig.Validate field.State }
            )

        let isValid =
            newFields
            |> List.filter (fun field ->
                let fieldConfig = Map.find field.Type config.FieldsConfig
                not (fieldConfig.IsValid field.State)
            )
            |> List.length
            |> (=) 0

        { form with Fields = newFields }, isValid

    /// Generate a JSON representation from the current state
    let toJson (config : Config<_>) (form : State) : string =
        form.Fields
        |> List.map (fun field ->
            let fieldConfig = Map.find field.Type config.FieldsConfig
            fieldConfig.ToJson field.State
        )
        |> Encode.object
        #if DEBUG
        |> Encode.toString 4
        #else
        |> Encode.toString 0
        #endif

    /// Set the loading state of the form
    let setLoading (isLoading : bool) (form : State) : State =
        { form with IsLoading = isLoading }

    /// Check if the form is loading
    /// Returns:
    /// - `true` if the form is marked as loading
    /// - `false` otherwise
    let isLoading (form : State) = form.IsLoading

    /// Set error for each field based on a `ErrorDef list`
    /// You can use this function in order to set errors coming from your server
    let setErrors (config : Config<_>) (errors : ErrorDef list) (form : State) =
        let errors =
            errors
            |> List.groupBy (fun error ->
                error.Key
            )
            |> Map.ofList

        let newFields =
            form.Fields
            |> List.map (fun field ->
                match errors.TryGetValue field.Name with
                | (true, first::_) ->
                    let fieldConfig = Map.find field.Type config.FieldsConfig
                    { field with State = fieldConfig.SetError field.State first.Text }
                | (true, [])
                | (false, _) -> field
            )

        { form with Fields = newFields }
