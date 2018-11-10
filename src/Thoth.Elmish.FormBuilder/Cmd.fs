namespace Thoth.Elmish.FormBuilder

open Elmish
open Types
open Fable.PowerPack

[<RequireQualifiedAccess>]
module Cmd =

    let none (_id : FieldId) : Cmd<Msg> =
        [ ]

    let ofPromise (task: 'a -> Fable.Import.JS.Promise<'FieldMessage>)
                  (arg:'a)
                  (fieldId : FieldId) : Cmd<Msg> =

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
