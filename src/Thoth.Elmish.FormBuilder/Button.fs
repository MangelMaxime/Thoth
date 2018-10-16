namespace Thoth.Elmish.FormBuilder

open Fulma
open Fable.Helpers.React

module Button =

    type ButtonState<'AppMsg> =
        { Label : string
          OnClick : 'AppMsg option
          IsPrimary : bool }

    type ButtonBuilder() =
        member __.Bind(m, f) =
            failwith "bind"

        member __.Return x = failwith "return"

        member __.ReturnForm() = failwith "return from"

        member __.Yield( x ) =
            { Label = ""
              OnClick = None
              IsPrimary = false }

        [<CustomOperation("onClick")>]
        member __.OnClick (buttonState : ButtonState<_>, onClick) =
            { buttonState with OnClick = Some onClick }

        [<CustomOperation("label")>]
        member __.Label (buttonState : ButtonState<_>, label) =
            { buttonState with Label = label }

        [<CustomOperation("isPrimary")>]
        member __.IsPrimary (buttonState : ButtonState<_>) =
            { buttonState with IsPrimary = true }

    let render dispatch (buttonState : ButtonState<_>) =
        Control.div [ ]
            [ Button.button [ if buttonState.OnClick.IsSome then
                                yield Button.OnClick (fun _ -> dispatch buttonState.OnClick.Value )
                              if buttonState.IsPrimary then
                                yield Button.Color IsPrimary ]
                [ str buttonState.Label ] ]
