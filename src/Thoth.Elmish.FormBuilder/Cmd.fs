namespace Thoth.Elmish.FormBuilder

open Elmish
open Types
open Fable.PowerPack
open System

[<RequireQualifiedAccess>]
module Cmd =

    let none (_fieldGuid : Guid) : Cmd<Msg> =
        [ ]

    let ofPromise (task: 'a -> Fable.Import.JS.Promise<'FieldMessage>)
                  (arg:'a)
                  (fieldGuid : Guid) : Cmd<Msg> =

        let bind dispatch =
            task arg
            |> Promise.map (fun fieldMessage ->
                OnFieldMessage (fieldGuid, fieldMessage)
                |> dispatch
            )
            |> Promise.catch (fun error ->
                OnFieldMessage (fieldGuid, error)
                |> dispatch
            )
            |> ignore

        [ bind ]
