namespace Thoth.Elmish.FormBuilder

open Elmish
open Types
open Fable.PowerPack
open System

[<RequireQualifiedAccess>]
module FormCmd =

    let inline internal mapMessage (fieldName : FieldName) (fieldMessage : 'FieldMessage) =
        OnFieldMessage (fieldName, fieldMessage)

    /// None - no commands, also known as `[]`
    let none (_fieldName : FieldName) : Cmd<Msg> =
        [ ]

    /// Command to issue a specific message
    let ofMsg (fieldMessage:'FieldMessage) (fieldName : FieldName) : Cmd<Msg> =
        [ fun dispatch ->
            OnFieldMessage (fieldName, fieldMessage)
            |> dispatch ]

    /// When emitting the message, map to another type
    let map (f: 'a -> 'FieldMessage) (cmd: Cmd<'a>) (fieldName : FieldName) : Cmd<Msg> =
        cmd |> List.map (fun g -> (fun dispatch -> f >> (mapMessage fieldName) >> dispatch) >> g)

    /// Aggregate multiple commands
    let batch (cmds: #seq<Cmd<Msg>>) (_fieldName : FieldName) : Cmd<Msg> =
        cmds |> List.concat

    /// Command that will evaluate an async block and map the result
    /// into success or error (of exception)
    let ofAsync (task: 'a -> Async<_>)
                (arg: 'a)
                (ofSuccess: _ -> 'FieldMessage)
                (ofError: _ -> 'FieldMessage)
                (fieldName : FieldName) : Cmd<Msg> =
        let bind dispatch =
            async {
                let! r = task arg |> Async.Catch
                dispatch (match r with
                         | Choice1Of2 x -> x |> (ofSuccess >> (mapMessage fieldName))
                         | Choice2Of2 x -> x |> (ofError >> (mapMessage fieldName)))
            }
        [ bind >> Async.StartImmediate ]

    /// Command to evaluate a simple function and map the result
    /// into success or error (of exception)
    let ofFunc (task: 'a -> _)
               (arg: 'a) (ofSuccess: _ -> 'FieldMessage)
               (ofError: _ -> 'FieldMessage)
               (fieldName : FieldName) : Cmd<Msg> =
        let bind dispatch =
            try
                task arg
                |> (ofSuccess >> (mapMessage fieldName) >> dispatch)
            with x ->
                x |> (ofError >> (mapMessage fieldName) >> dispatch)
        [ bind ]

    /// Command to evaluate a simple function and map the success to a message
    /// discarding any possible error
    let performFunc (task: 'a -> _)
                    (arg: 'a)
                    (ofSuccess: _ -> 'FieldMessage)
                    (fieldName : FieldName) : Cmd<Msg> =
        let bind dispatch =
            try
                task arg
                |> (ofSuccess >> (mapMessage fieldName) >> dispatch)
            with _ ->
                ()
        [ bind ]

    /// Command to evaluate a simple function and map the error (in case of exception)
    let attemptFunc (task: 'a -> unit)
                    (arg: 'a)
                    (ofError: _ -> 'FieldMessage)
                    (fieldName : FieldName) : Cmd<Msg> =
        let bind dispatch =
            try
                task arg
            with x ->
                x |> (ofError >> (mapMessage fieldName) >> dispatch)
        [ bind ]

    open Fable.PowerPack

    /// Command to call `promise` block and map the results
    let ofPromise (task: 'a -> Fable.Import.JS.Promise<_>)
                  (arg:'a)
                  (ofSuccess: _ -> 'FieldMessage)
                  (ofError: _ -> 'FieldMessage)
                  (fieldName : FieldName) : Cmd<Msg> =
        let bind dispatch =
            task arg
            |> Promise.map (ofSuccess >> (mapMessage fieldName) >> dispatch)
            |> Promise.catch (ofError >> (mapMessage fieldName) >> dispatch)
            |> ignore

        [ bind ]
