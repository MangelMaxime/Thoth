namespace Thoth.Elmish.FormBuilder

[<AutoOpen>]
module Common =

    type ValidationState=
        | Valid
        | Invalid of string

        member this.ToText
            with get () =
                match this with
                | Valid -> ""
                | Invalid msg -> msg
