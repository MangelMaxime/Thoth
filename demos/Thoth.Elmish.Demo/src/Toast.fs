module Demos.Toast

open Elmish
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fulma
open Thoth.Elmish
open Fulma.FontAwesome
open Fulma.Extensions
open System
open Fable.Import
open Fable.Core

module CopyButton =

    open Fable.Core.JsInterop

    type Props =
        | Value of string

    let inline copyButtton (props: Props list) : React.ReactElement =
        ofImport "default" "./js/CopyButton.js" (keyValueList CaseRules.LowerFirst props) []

let renderToastWithFulma =
    { new Toast.IRenderer<Fa.I.FontAwesomeIcons> with
        member __.Toast children color =
            Notification.notification [ Notification.CustomClass color ]
                children
        member __.CloseButton onClick =
            Notification.delete [ Props [ OnClick onClick ] ]
                [ ]
        member __.InputArea children =
            Columns.columns [ Columns.IsGapless
                              Columns.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ]
                              Columns.CustomClass "notify-inputs-area" ]
                children
        member __.Input (txt : string) (callback : (unit -> unit)) =
            Column.column [ ]
                [ Button.button [ Button.OnClick (fun _ -> callback ())
                                  Button.Color IsWhite ]
                    [ str txt ] ]
        member __.Title txt =
            Heading.h5 []
                [ str txt ]
        member __.Icon (icon : Fa.I.FontAwesomeIcons) =
            Icon.faIcon [ Icon.Size IsMedium ]
                [ Fa.icon icon
                  Fa.fa2x ]
        member __.SingleLayout title message =
            div [ ]
                [ title; message ]
        member __.Message txt =
            span [ ]
                [ str txt ]
        member __.SplittedLayout iconView title message =
            Columns.columns [ Columns.IsGapless
                              Columns.IsVCentered ]
                [ Column.column [ Column.Width (Screen.All, Column.Is2) ]
                    [ iconView ]
                  Column.column [ ]
                    [ title
                      message ] ]
        member __.StatusToColor status =
            match status with
            | Toast.Success -> "is-success"
            | Toast.Warning -> "is-warning"
            | Toast.Error -> "is-danger"
            | Toast.Info -> "is-info" }

type State =
    | Initial
    | IsTyping
    | StoppedTyping

type Renderer =
    | Fulma
    | Default

type Model =
    { Position : Toast.Position
      Status : Toast.Status
      Message : string
      Title : string
      Group : string
      Icon : string
      Delay : string
      DelayError : string
      DismissOnClick : bool
      WithProgressBar : bool
      WithCloseButton : bool
      NoTimeOut : bool
      GlobalError : string
      Code : string
      Renderer : Renderer }

type Msg =
    | DemoInfo
    | DemoWarning
    | DemoSuccess
    | DemoError
    | ChangePosition of Toast.Position
    | ChangeStatus of Toast.Status
    | ChangeMessage of string
    | ChangeTitle of string
    | ChangeGroup of string
    | ChangeIcon of string
    | ChangeDelay of string
    | ToggleDismissOnClick
    | ToggleWithProgressBar
    | ToggleWithCloseButton
    | ToggleNoTimeOut
    | ShowToast
    | RefreshCode

let private init _ =
    let renderer =
        let search = Browser.window.location.search
        if search.Length = 0 || search.Contains("fulma") then
            Renderer.Fulma
        else
            Renderer.Default

    { Position = Toast.BottomLeft
      Status = Toast.Info
      Message = "Hello, I am a toast"
      Title = ""
      Group = ""
      Icon = ""
      Delay = "3"
      DelayError = ""
      DismissOnClick = false
      WithProgressBar = false
      WithCloseButton = false
      NoTimeOut  = false
      GlobalError = ""
      Code = ""
      Renderer = renderer }, Cmd.ofMsg ShowToast

type Step =
    { Model : Model
      Builder : Toast.Builder<string,Msg>
      CodeLines : string list }

    member this.ToCode () =
        String.concat "\n|> " this.CodeLines

    static member ApplyBuilder builder  step =
        { step with Builder = builder step.Builder }

    static member AddCode code step =
        { step with CodeLines = step.CodeLines @ [ code ] }

let private buildMessage (model : Model) =
    { Model = model
      Builder = Toast.message model.Message
      CodeLines = [ sprintf "Toast.message \"%s\"" model.Message ] }

let private buildTitle (step : Step) =
    if step.Model.Title <> "" then
        step
        |> Step.ApplyBuilder (Toast.title step.Model.Title)
        |> Step.AddCode (sprintf "Toast.title \"%s\"" step.Model.Title)
    else
        step

let private buildGroup (step : Step) =
    if step.Model.Group <> "" then
        step
        |> Step.ApplyBuilder (Toast.group step.Model.Group)
        |> Step.AddCode (sprintf "Toast.group \"%s\"" step.Model.Group)
    else
        step

let private buildPosition (step : Step) =
    step
    |> Step.ApplyBuilder (Toast.position step.Model.Position)
    |> Step.AddCode (sprintf "Toast.position Toast.%s" (step.Model.Position.ToString()))

let private buildStatus (step : Step) =
    let builder, step =
        match step.Model.Status with
        | Toast.Success -> Toast.success,  Step.AddCode "Toast.success" step
        | Toast.Info -> Toast.info, Step.AddCode "Toast.info" step
        | Toast.Warning -> Toast.warning, Step.AddCode "Toast.warning" step
        | Toast.Error -> Toast.error, Step.AddCode "Toast.error" step

    builder step.Builder, step.ToCode()

let private buildIcon (step : Step) =
    if step.Model.Icon <> "" then
        step
        |> Step.ApplyBuilder (Toast.icon step.Model.Icon)
        |> Step.AddCode (sprintf "Toast.icon \"%s\"" step.Model.Icon)
    else
        step

let private buildDelay (step : Step) =
    if step.Model.NoTimeOut then
        step
        |> Step.ApplyBuilder Toast.noTimeout
        |> Step.AddCode "Toast.noTimeout"
    else
        step
        |> Step.ApplyBuilder (Toast.timeout (TimeSpan.FromSeconds (float step.Model.Delay)))
        |> Step.AddCode (sprintf "Toast.timeout (TimeSpan.FromSeconds (%.1f))" (float step.Model.Delay))

let private buildDismissOnClick (step : Step) =
    if step.Model.DismissOnClick then
        step
        |> Step.ApplyBuilder Toast.dismissOnClick
        |> Step.AddCode "Toast.dismissOnClick"
    else
        step

// let private buildWithProgressBar (step : Step) =
//     if step.Model.WithProgressBar then
//         step
//         |> Step.ApplyBuilder Toast.withProgessBar
//         |> Step.AddCode "Toast.withProgessBar"
//     else
//         step

let private buildWithCloseButton (step : Step) =
    if step.Model.WithCloseButton then
        step
        |> Step.ApplyBuilder Toast.withCloseButton
        |> Step.AddCode "Toast.withCloseButton"
    else
        step

let private update msg model =
    match msg with
    | DemoInfo ->
        model, Toast.message "I am toast of type Info"
                |> Toast.title "Info"
                |> Toast.info

    | DemoWarning ->
        model, Toast.message "I am toast of type Warning"
                |> Toast.title "Warning"
                |> Toast.warning

    | DemoSuccess ->
        model, Toast.message "I am toast of type Success"
                |> Toast.title "Success"
                |> Toast.success

    | DemoError ->
        model, Toast.message "I am toast of type Error"
                |> Toast.title "Error"
                |> Toast.error

    | ChangePosition newPosition ->
        { model with Position = newPosition }, Cmd.ofMsg RefreshCode

    | ChangeStatus newStatus ->
        { model with Status = newStatus }, Cmd.ofMsg RefreshCode

    | ChangeMessage newMessage ->
        { model with Message = newMessage }, Cmd.ofMsg RefreshCode

    | ChangeTitle newTitle ->
        { model with Title = newTitle }, Cmd.ofMsg RefreshCode
    
    | ChangeGroup newGroup ->
        { model with Group = newGroup }, Cmd.ofMsg RefreshCode

    | ChangeIcon newIcon ->
        { model with Icon = newIcon }, Cmd.ofMsg RefreshCode

    | ChangeDelay newDelay ->
        let errorMsg =
            try
                float newDelay |> ignore
                ""
            with
                | _ -> "Invalid float value"

        { model with Delay = newDelay
                     DelayError = errorMsg }, Cmd.ofMsg RefreshCode

    | ToggleDismissOnClick ->
        { model with DismissOnClick = not model.DismissOnClick }, Cmd.ofMsg RefreshCode

    | ToggleWithProgressBar ->
        { model with WithProgressBar = not model.WithProgressBar }, Cmd.ofMsg RefreshCode

    | ToggleWithCloseButton ->
        { model with WithCloseButton = not model.WithCloseButton }, Cmd.ofMsg RefreshCode

    | ToggleNoTimeOut ->
        { model with NoTimeOut = not model.NoTimeOut }, Cmd.ofMsg RefreshCode

    | ShowToast ->
        if not model.NoTimeOut && model.DelayError <> "" then
            { model with GlobalError = "Please fix the errors first" }, Cmd.none
        else
            let (toastCmd, code) =
                model
                |> buildMessage
                |> buildTitle
                |> buildGroup
                |> buildPosition
                |> buildIcon
                |> buildDelay
                |> buildDismissOnClick
                // |> buildWithProgressBar
                |> buildWithCloseButton
                |> buildStatus

            { model with GlobalError = ""
                         Code = code }, toastCmd

    | RefreshCode ->
        if model.DelayError <> "" && not model.NoTimeOut then
            model, Cmd.none
        else
            let (_, code) =
                model
                |> buildMessage
                |> buildTitle
                |> buildGroup
                |> buildPosition
                |> buildIcon
                |> buildDelay
                |> buildDismissOnClick
                // |> buildWithProgressBar
                |> buildWithCloseButton
                |> buildStatus

            { model with Code = code }, Cmd.none


let private positions =
    [ Toast.BottomRight
      Toast.BottomLeft
      Toast.BottomCenter
      Toast.TopRight
      Toast.TopLeft
      Toast.TopCenter ]

type Toast.Position with
    member this.ToString () =
        match this with
        | Toast.BottomRight -> "BottomRight"
        | Toast.BottomLeft -> "BottomLeft"
        | Toast.BottomCenter -> "BottomCenter"
        | Toast.TopRight -> "TopRight"
        | Toast.TopLeft -> "TopLeft"
        | Toast.TopCenter -> "TopCenter"

let private checkradioPositions activePosition dispatch =
    positions
    |> List.map (fun position ->
        Checkradio.radio [ Checkradio.Name "toast-position"
                           Checkradio.Checked (activePosition = position)
                           Checkradio.OnChange (fun _ -> dispatch (ChangePosition position)) ]
            [ str (position.ToString()) ]
    )

let private statuses =
    [ Toast.Success, IsSuccess
      Toast.Warning, IsWarning
      Toast.Error, IsDanger
      Toast.Info, IsInfo ]

let private checkradioStatuses activeStatus dispatch =
    statuses
    |> List.map (fun (status, color) ->
        Checkradio.radio [ Checkradio.IsCircle
                           Checkradio.HasBackgroundColor
                           Checkradio.Name "toast-type"
                           Checkradio.Color color
                           Checkradio.Checked (activeStatus = status)
                           Checkradio.OnChange (fun _ -> dispatch (ChangeStatus status)) ]
            [ str (status.ToString()) ]
    )

let private timeoutHelper (model : Model) =
    match model.NoTimeOut with
    | true ->
        Help.help [ Help.Color IsInfo ]
            [ if model.NoTimeOut then
                yield str "Uncheck 'No timeout', to set a delay" ]
    | false ->
      Help.help [ Help.Color IsDanger ]
        [ str model.DelayError ]

let private viewCode (model : Model) =
    Column.column [ Column.Width (Screen.Tablet, Column.Is5) ]
        [ Text.div [ Modifiers [ Modifier.TextAlignment (Screen.Tablet, TextAlignment.Centered) ] ]
            [ Heading.h3 [ ]
                [ str "Code preview" ]
              Heading.p [ Heading.IsSubtitle
                          Heading.Is6 ]
                [ str "Refreshed when you click on 'Show toast'" ] ]
          br [ ]
          Content.content [ Content.Size IsMedium ]
            [ pre [ ]
                [ code [ ]
                    [ str model.Code ] ] ]
          Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
            [ CopyButton.copyButtton [ CopyButton.Value model.Code ] ] ]

let private viewBuilder (model : Model) dispatch =
    Column.column [ Column.Width (Screen.Tablet, Column.Is5)
                    Column.Offset (Screen.Tablet, Column.Is1) ]
        [ Heading.h3 [ Heading.Modifiers [ Modifier.TextAlignment (Screen.Tablet, TextAlignment.Centered) ] ]
            [ str "Interactive builder" ]
          Field.div [ ]
            [ Label.label [ ]
                [ str "Title" ]
              Control.div [ ]
                [ Input.text [ Input.Value model.Title
                               Input.OnChange (fun ev -> dispatch (ChangeTitle ev.Value)) ] ]
              Help.help [ ]
                [ str "If empty, no title will be added to the Toast" ] ]
          Field.div [ ]
            [ Label.label [ ]
                [ str "Group" ]
              Control.div [ ]
                [ Input.text [ Input.Value model.Group
                               Input.OnChange (fun ev -> dispatch (ChangeGroup ev.Value)) ] ]
              Help.help [ ]
                [ str "If empty, no group will be added to the Toast" ] ]
          Field.div [ ]
            [ Label.label [ ]
                [ str "Message" ]
              Control.div [ ]
                [ Input.text [ Input.Value model.Message
                               Input.OnChange (fun ev -> dispatch (ChangeMessage ev.Value)) ] ] ]
          Columns.columns [  ]
            [ Column.column [ ]
                [ Field.div [ ]
                    [ Label.label [ ]
                        [ str "Icon"
                          Icon.faIcon [ Icon.Props [ Tooltip.dataTooltip "Demo only support Font Awesome" ]
                                        Icon.CustomClass Tooltip.ClassName ]
                            [ Fa.icon Fa.I.QuestionCircleO ] ]
                      Control.div [ ]
                        [ Input.text [ Input.Value model.Icon
                                       Input.OnChange (fun ev -> dispatch (ChangeIcon ev.Value)) ] ]
                      Help.help [ ]
                        [ str "If empty, no icon will be added to the Toast" ] ] ]
              Column.column [ ]
                [ Field.div [ ]
                    [ Label.label [ ]
                        [ str "Timeout delay" ]
                      Control.div [ ]
                        [ Input.number [ Input.Value model.Delay
                                         Input.Disabled model.NoTimeOut
                                         Input.OnChange (fun ev -> dispatch (ChangeDelay ev.Value)) ] ]
                      timeoutHelper model ] ] ]
          Columns.columns [  ]
            [ Column.column [ ]
                [ Field.div [ ]
                    [ Label.label [ ]
                        [ str "Type" ]
                      Control.div [ ]
                        (checkradioStatuses model.Status dispatch) ] ]
              Column.column [ ]
                [ Field.div [ ]
                    [ Label.label [ ]
                        [ str "Position" ]
                      Control.div [ ]
                        (checkradioPositions model.Position dispatch) ] ] ]
          Columns.columns [ Columns.IsMultiline
                            Columns.Props [ Style [ WhiteSpace "nowrap" ] ] ]
             [
            //    Column.column [ ]
            //     [ Switch.switch [ Switch.Checked model.WithProgressBar
            //                       Switch.OnChange (fun _ -> dispatch ToggleWithProgressBar) ]
            //         [ str "With progress bar" ] ]
               Column.column [ ]
                [ Switch.switch [ Switch.Checked model.WithCloseButton
                                  Switch.OnChange (fun _ -> dispatch ToggleWithCloseButton) ]
                    [ str "With close button" ] ]
               Column.column [ ]
                [ Switch.switch [ Switch.Checked model.DismissOnClick
                                  Switch.OnChange (fun _ -> dispatch ToggleDismissOnClick) ]
                    [ str "Dismiss on click" ] ]
               Column.column [ ]
                [ Switch.switch [ Switch.Checked model.NoTimeOut
                                  Switch.OnChange (fun _ -> dispatch ToggleNoTimeOut) ]
                    [ str "No timeout" ] ] ]
          Field.div [ ]
            [ Text.p [ Modifiers [ Modifier.TextColor IsDanger
                                   Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                [ str model.GlobalError ] ]
          Field.div [ Field.IsGroupedCentered ]
            [ Control.p [ ]
                [ Button.button [ Button.Color IsPrimary
                                  Button.OnClick (fun _ -> dispatch ShowToast) ]
                    [ str "Show Toast" ] ] ] ]

let private switchRenderer (renderer : Renderer) (_: React.MouseEvent) =
    match renderer with
    | Renderer.Default -> Browser.window.location.search <- "?renderer=default"
    | Renderer.Fulma -> Browser.window.location.search <- "?renderer=fulma"

let private view model dispatch =
    div [ ]
        [ Section.section [ Section.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
            [ Heading.h5 [ ]
                [ str "Available renderer" ]
              Heading.p [ Heading.IsSubtitle
                          Heading.Is6 ]
                [ str "You can click on a renderer to set it as active" ]
              Button.list [ Button.List.IsCentered ]
                [ Button.button [ if model.Renderer = Renderer.Default then
                                    yield Button.Color IsInfo
                                  yield Button.OnClick (switchRenderer Renderer.Default) ]
                    [ str "Default" ]
                  Button.button [ if model.Renderer = Renderer.Fulma then
                                    yield Button.Color IsInfo
                                  yield Button.OnClick (switchRenderer Renderer.Fulma) ]
                    [ str "Fulma" ] ] ]
          Columns.columns [ ]
            [ viewBuilder model dispatch
              viewCode model ] ]

open Elmish.React
open Elmish.Debug
open Elmish.HMR
open Toast


// This function is just for the demonstration usage
// It's allow us to switch the renderer used based on the URL params
let chooseRenderer =
    let search = Browser.window.location.search
    if search.Length = 0 || search.Contains("fulma") then
        renderToastWithFulma :?> IRenderer<string>
    else
        Toast.render

let start (id : string) =
    Program.mkProgram init update view
    |> Program.withToast chooseRenderer
    |> Program.withReactUnoptimized id
    |> Program.run
