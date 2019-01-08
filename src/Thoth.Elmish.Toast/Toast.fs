namespace Thoth.Elmish

module Toast =

    open Fable.Import
    open System
    open Fable.Helpers.React
    open Fable.Helpers.React.Props
    open Elmish
    open Fable.Core.JsInterop

    importSideEffects "./css/toast-base.css"
    importSideEffects "./css/toast-minimal.css"

    let eventIdentifier = "thoth_elmish_toast_notify_event"

    type Status =
        | Success
        | Warning
        | Error
        | Info

    type Position =
        | BottomRight
        | BottomLeft
        | BottomCenter
        | TopRight
        | TopLeft
        | TopCenter

    type Builder<'icon, 'msg> =
        { Inputs : (string * 'msg) list
          Message : string
          Title : string option
          Group : string option
          Icon : 'icon option
          Position : Position
          Delay : TimeSpan option
          DismissOnClick : bool
          WithProgressBar : bool
          WithCloseButton : bool }

        static member Empty () =
            { Inputs = []
              Message = ""
              Title = None
              Group = None
              Icon = None
              Delay = Some (TimeSpan.FromSeconds 3.)
              Position = BottomLeft
              DismissOnClick = false
              WithProgressBar = false
              WithCloseButton = false }

    type Toast<'icon> =
        { Guid : Guid
          Inputs : (string * (unit -> unit)) list
          Message : string
          Title : string option
          Group : string option
          Icon : 'icon option
          Position : Position
          Delay : TimeSpan option
          Status : Status
          DismissOnClick : bool
          WithProgressBar : bool
          WithCloseButton : bool }

    /// Create a toast and set the message content
    let message msg =
        { Builder<_, _>.Empty() with Message = msg }

    /// Set the title content
    let title title (builder : Builder<_, _>) =
        { builder with Title = Some title }

    /// Set the group name
    let group group (builder : Builder<_, _>) =
        { builder with Group = Some group }

    /// Set the position
    let position pos (builder : Builder<_, _>) =
        { builder with Position = pos }

    /// Add an input to the toast
    let addInput txt msg (builder : Builder<_, _>) =
        { builder with Inputs = (txt, msg) :: builder.Inputs }

    /// Set the icon
    let icon icon (builder : Builder<_, _>) =
        { builder with Icon = Some icon }

    /// Set the timeout in seconds
    let timeout delay (builder : Builder<_, _>) =
        { builder with Delay = Some delay }

    /// No timeout, make sure to add close button or dismiss on click
    let noTimeout (builder : Builder<_, _>) =
        { builder with Delay = None }

    /// Allow user to dismiss the toast by cliking on it
    let dismissOnClick (builder : Builder<_, _>) =
        { builder with DismissOnClick = true }

    /// Add an animated progress bar
    // let withProgessBar (builder : Builder<_, _>) =
    //     { builder with WithProgressBar = true }

    /// Add a close button
    let withCloseButton (builder : Builder<_, _>) =
        { builder with WithCloseButton = true }

    let private triggerEvent (builder : Builder<_, _>) status dispatch =
        let detail =
            jsOptions<Browser.CustomEventInit>(fun o ->
                o.detail <-
                    Some (box { Guid = Guid.NewGuid()
                                Inputs =
                                    builder.Inputs
                                    |> List.map (fun (txt, msg) ->
                                        txt, fun () -> dispatch msg
                                    )
                                Message = builder.Message
                                Title = builder.Title
                                Group = builder.Group
                                Icon = builder.Icon
                                Position = builder.Position
                                Delay = builder.Delay
                                Status = status
                                DismissOnClick = builder.DismissOnClick
                                WithProgressBar = builder.WithProgressBar
                                WithCloseButton = builder.WithCloseButton })
            )
        let event = Browser.CustomEvent.Create(eventIdentifier, detail)
        Browser.window.dispatchEvent(event)
        |> ignore

    /// Send the toast marked with Success status
    let success (builder : Builder<_, _>) : Cmd<'msg> =
        [ fun dispatch ->
            triggerEvent builder Success dispatch ]

    /// Send the toast marked with Warning status
    let warning (builder : Builder<_, _>) : Cmd<'msg> =
        [ fun dispatch ->
            triggerEvent builder Warning dispatch ]

    /// Send the toast marked with Error status
    let error (builder : Builder<_, _>) : Cmd<'msg> =
        [ fun dispatch ->
            triggerEvent builder Error dispatch ]

    /// Send the toast marked with Info status
    let info (builder : Builder<_, _>) : Cmd<'msg> =
        [ fun dispatch ->
            triggerEvent builder Info dispatch ]

    /// Interface used to customize the view
    type IRenderer<'icon> =

        /// **Description**
        /// Render the outer element of the toast
        /// **Parameters**
        /// * `content` - parameter of type `React.ReactElement list`
        ///     > This is the content of the toast.
        ///     > Ex:
        ///     >   - CloseButton
        ///     >   - Title
        ///     >   - Message
        /// * `color` - parameter of type `string`
        ///     > Class used to set toast color
        /// **Output Type**
        ///   * `React.ReactElement`
        abstract Toast : React.ReactElement list -> string -> React.ReactElement

        /// **Description**
        /// Render the close button of the toast
        /// **Parameters**
        /// * `onClick` - parameter of type `React.MouseEvent -> unit`
        ///     > OnClick event listener to attached
        /// **Output Type**
        ///   * `React.ReactElement`
        abstract CloseButton : (React.MouseEvent -> unit) -> React.ReactElement

        /// **Description**
        /// Render the outer element of the Input Area
        /// **Parameters**
        /// * `content` - parameter of type `React.ReactElement list`
        ///     > This is the content of the input area.
        /// **Output Type**
        ///   * `React.ReactElement`
        abstract InputArea : React.ReactElement list -> React.ReactElement

        /// **Description**
        /// Render one element of the Input Area
        /// **Parameters**
        /// * `text` - parameter of type `string`
        ///     > Text to display
        /// * `callback` - parameter of type `unit -> unit`
        ///     > Callback to execute when user click on the input
        /// **Output Type**
        ///   * `React.ReactElement`
        abstract Input : string -> (unit -> unit) -> React.ReactElement

        /// **Description**
        /// Render the title of the Toast
        /// **Parameters**
        /// * `text` - parameter of type `string`
        ///     > Text to display
        /// **Output Type**
        ///   * `React.ReactElement`
        abstract Title : string -> React.ReactElement

        /// **Description**
        /// Render the message of the Toast
        /// **Parameters**
        /// * `text` - parameter of type `string`
        ///     > Text to display
        /// **Output Type**
        ///   * `React.ReactElement`
        abstract Message : string -> React.ReactElement

        /// **Description**
        /// Render the icon part
        /// **Parameters**
        /// * `icon` - parameter of type `'icon`
        ///     > 'icon is generic so you can pass the Value as a String or Typed value like `Fa.I.FontAwesomeIcons` when using Fulma
        /// **Output Type**
        ///   * `React.ReactElement`
        abstract Icon : 'icon -> React.ReactElement

        /// **Description**
        /// Render the simple layout (when no icon has been provided to the Toast)
        /// **Parameters**
        /// * `title` - parameter of type `React.ReactElement`
        /// * `message` - parameter of type `React.ReactElement`
        /// **Output Type**
        ///   * `React.ReactElement`
        abstract SingleLayout : React.ReactElement -> React.ReactElement -> React.ReactElement


        /// **Description**
        /// Render the splitted layout (when toast has an Icon and Message)
        /// **Parameters**
        /// * `icon` - parameter of type `React.ReactElement`
        ///     > Icon view
        /// * `title` - parameter of type `React.ReactElement`
        /// * `message` - parameter of type `React.ReactElement`
        /// **Output Type**
        ///   * `React.ReactElement`
        abstract SplittedLayout : React.ReactElement -> React.ReactElement -> React.ReactElement -> React.ReactElement

        /// **Description**
        /// Obtain the class associated with the Status
        /// **Parameters**
        /// * `status` - parameter of type `Status`
        /// **Output Type**
        ///   * `string`
        abstract StatusToColor : Status -> string

    [<RequireQualifiedAccess>]
    module Program =

        type Notifiable<'icon, 'msg> =
            | Add of Toast<'icon>
            | Remove of Toast<'icon>
            | UserMsg of 'msg
            | OnError of exn

        type Model<'icon, 'model> =
            { UserModel : 'model
              Toasts_BL : Toast<'icon> list
              Toasts_BC : Toast<'icon> list
              Toasts_BR : Toast<'icon> list
              Toasts_TL : Toast<'icon> list
              Toasts_TC : Toast<'icon> list
              Toasts_TR : Toast<'icon> list }


        let inline addToast newToast toasts = 
            let shouldKeep toast = 
                newToast.Group |> Option.isNone
                ||
                newToast.Group <> toast.Group

            newToast::(toasts |> List.filter shouldKeep)

        let inline private removeToast guid =
            List.filter (fun item -> item.Guid <> guid )

        let private viewToastWrapper (classPosition : string) (render : IRenderer<_>) (toasts : Toast<_> list) dispatch =
            div [ Class ("toast-wrapper " + classPosition) ]
                ( toasts
                        |> List.map (fun n ->
                            let title =
                                Option.map
                                    render.Title
                                    n.Title

                            let withInputArea, inputArea =
                                if n.Inputs.Length = 0 then
                                    "", None
                                else
                                    let inputs =
                                        render.InputArea
                                            (n.Inputs
                                                |> List.map (fun (txt, callback) ->
                                                    render.Input txt callback
                                                ))

                                    "with-inputs", Some inputs

                            let dismissOnClick =
                                if n.DismissOnClick then
                                    "dismiss-on-click"
                                else
                                    ""

                            let containerClass =
                                String.concat " " [ "toast-container"
                                                    dismissOnClick
                                                    withInputArea
                                                    render.StatusToColor n.Status ]
                            let closeButton =
                                match n.WithCloseButton with
                                | true ->
                                    render.CloseButton (fun _ -> dispatch (Remove n))
                                    |> Some
                                | false -> None
                                |> ofOption

                            let layout =
                                match n.Icon with
                                | Some icon ->
                                    render.SplittedLayout
                                        (render.Icon icon)
                                        (ofOption title)
                                        (render.Message n.Message)
                                | None ->
                                    render.SingleLayout
                                        (ofOption title)
                                        (render.Message n.Message)

                            div [ yield ClassName containerClass :> IHTMLProp
                                  if n.DismissOnClick then
                                       yield OnClick (fun _ -> dispatch (Remove n)) :> IHTMLProp ]
                                [ render.Toast
                                    [ closeButton
                                      layout
                                    ]
                                    (render.StatusToColor n.Status)
                                  ofOption inputArea ]
                        ) )

        let private view  (render : IRenderer<_>) (model : Model<_, _>) dispatch =
            div [ Class "elmish-toast" ]
                [ viewToastWrapper "toast-wrapper-bottom-left" render model.Toasts_BL dispatch
                  viewToastWrapper "toast-wrapper-bottom-center" render model.Toasts_BC dispatch
                  viewToastWrapper "toast-wrapper-bottom-right" render model.Toasts_BR dispatch
                  viewToastWrapper "toast-wrapper-top-left" render model.Toasts_TL dispatch
                  viewToastWrapper "toast-wrapper-top-center" render model.Toasts_TC dispatch
                  viewToastWrapper "toast-wrapper-top-right" render model.Toasts_TR dispatch ]


        let private delayedCmd (notification : Toast<'icon>) =
            match notification.Delay with
            | Some delay ->
                promise {
                    do! Promise.sleep (int delay.TotalMilliseconds)
                    return notification
                }
            | None -> failwith "No delay attached to notification can't delayed it. `delayedCmd` should not have been called by the program"

        let withToast (renderer : IRenderer<'icon>) (program : Elmish.Program<'arg, 'model, 'msg, 'view >) =

            let update msg model =
                let newModel,cmd =
                    match msg with
                    | UserMsg msg ->
                        let newModel, cmd = program.update msg model.UserModel
                        { model with UserModel = newModel }, Cmd.map UserMsg cmd

                    | Add newToast ->
                        let cmd : Cmd<Notifiable<'icon, 'msg>>=
                            match newToast.Delay with
                            | Some _ -> Cmd.ofPromise delayedCmd newToast Remove OnError
                            | None -> Cmd.none

                        match newToast.Position with
                        | BottomLeft -> { model with Toasts_BL = addToast newToast model.Toasts_BL }, cmd
                        | BottomCenter -> { model with Toasts_BC = addToast newToast model.Toasts_BC }, cmd
                        | BottomRight -> { model with Toasts_BR = addToast newToast model.Toasts_BR }, cmd
                        | TopLeft -> { model with Toasts_TL = addToast newToast model.Toasts_TL }, cmd
                        | TopCenter -> { model with Toasts_TC = addToast newToast model.Toasts_TC }, cmd
                        | TopRight -> { model with Toasts_TR = addToast newToast model.Toasts_TR }, cmd

                    | Remove toast ->
                        match toast.Position with
                        | BottomLeft -> { model with Toasts_BL = removeToast toast.Guid model.Toasts_BL }, Cmd.none
                        | BottomCenter -> { model with Toasts_BC = removeToast toast.Guid model.Toasts_BC }, Cmd.none
                        | BottomRight -> { model with Toasts_BR = removeToast toast.Guid model.Toasts_BR }, Cmd.none
                        | TopLeft -> { model with Toasts_TL = removeToast toast.Guid model.Toasts_TL }, Cmd.none
                        | TopCenter -> { model with Toasts_TC = removeToast toast.Guid model.Toasts_TC }, Cmd.none
                        | TopRight -> { model with Toasts_TR = removeToast toast.Guid model.Toasts_TR }, Cmd.none


                    | OnError error ->
                        Browser.console.error error.Message
                        model, Cmd.none

                newModel, cmd

            let createModel (model, cmd) =
                { UserModel = model
                  Toasts_BL = []
                  Toasts_BC = []
                  Toasts_BR = []
                  Toasts_TL = []
                  Toasts_TC = []
                  Toasts_TR = [] }, cmd

            let notificationEvent (dispatch : Elmish.Dispatch<Notifiable<_, _>>) =
                // If HMR support is active, then we provide have a custom implementation.
                // This is needed to avoid:
                // - flickering (trigger several react renderer process)
                // - attaching several event listener to the same event
                #if DEBUG
                if not (isNull HMR.``module``.hot) then
                    if HMR.``module``.hot.status() <> HMR.Idle then
                        Browser.window.removeEventListener(eventIdentifier, !!Browser.window?(eventIdentifier))

                    Browser.window?(eventIdentifier) <- fun (ev : Browser.Event) ->
                        let ev = ev :?> Browser.CustomEvent
                        dispatch (Add (unbox ev.detail))

                    Browser.window.addEventListener(eventIdentifier, !!Browser.window?(eventIdentifier))
                else
                #endif
                    Browser.window.addEventListener(eventIdentifier, !^(fun ev ->
                        let ev = ev :?> Browser.CustomEvent
                        dispatch (Add (unbox ev.detail))
                    ))

            let init =
                program.init
                    >> (fun (model, cmd) ->
                            model, cmd |> Cmd.map UserMsg) >> createModel

            let subs model =
                Cmd.batch [ [ notificationEvent ]
                            program.subscribe model.UserModel |> Cmd.map UserMsg ]

            { init = init
              update = update
              subscribe = subs
              onError = program.onError
              setState = fun model dispatch -> program.setState model.UserModel (UserMsg >> dispatch)
              view = fun model dispatch ->
                div [ ]
                    [ view renderer model dispatch
                      program.view model.UserModel (UserMsg >> dispatch) ]
              // syncDispatch is only intended for .NET
              syncDispatch = id }

    /// **Description**
    /// Default implementation for the Toast renderer,
    /// you are encourage to write your own implementation
    /// to match your application style
    /// **Output Type**
    ///   * `IRenderer<string>`
    let render =
        { new IRenderer<string> with
            member __.Toast children _ =
                div [ Class "toast" ]
                    children
            member __.CloseButton onClick =
                span [ Class "close-button"
                       OnClick onClick ]
                    [ ]
            member __.InputArea children =
                div [ ]
                    [ str "Not implemented yet" ]
            member __.Input (txt : string) (callback : (unit -> unit)) =
                div [ ]
                    [ str "Not implemented yet" ]
            member __.Title txt =
                span [ Class "toast-title" ]
                    [ str txt ]
            member __.Icon (icon : string) =
                div [ Class "toast-layout-icon" ]
                    [ i [ Class ("fa fa-2x " + icon) ]
                        [  ] ]
            member __.SingleLayout title message =
                div [ Class "toast-layout-content" ]
                    [ title; message ]
            member __.Message txt =
                span [ Class "toast-message" ]
                    [ str txt ]
            member __.SplittedLayout iconView title message =
                div [ Style [ Display "flex"
                              Width "100%" ] ]
                    [ iconView
                      div [ Class "toast-layout-content" ]
                        [ title
                          message ] ]
            member __.StatusToColor status =
                match status with
                | Status.Success -> "is-success"
                | Status.Warning -> "is-warning"
                | Status.Error -> "is-error"
                | Status.Info -> "is-info" }
