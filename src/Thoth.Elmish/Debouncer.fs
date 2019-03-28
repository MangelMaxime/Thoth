namespace Thoth.Elmish

[<RequireQualifiedAccess>]
module Debouncer =

    open System
    open Fable.Core
    open Elmish

    type Id = string

    type State =
        { PendingMessages : Map<Id, int> }

    let create () =
        { PendingMessages = Map.empty }

    type SelfMessage<'AppMsg> =
        | Timeout of id:Id * appMsg:'AppMsg
        | OnError of exn

    let bounce (delay : TimeSpan) (id: Id) (msgToSend: 'a) (currentState : State) =
        let counterInc =
            Option.map ((+) 1) >> Option.defaultValue 1

        let delayedCmd _ =
            promise {
                do! Promise.sleep (int delay.TotalMilliseconds)
                return (id, msgToSend)
            }

        let updatedState =
            let newCounter = Map.tryFind id currentState.PendingMessages |> counterInc

            { currentState
                with PendingMessages = Map.add id newCounter currentState.PendingMessages }

        updatedState, Cmd.OfPromise.either delayedCmd () Timeout OnError

    let update (selfMessage : SelfMessage<_>) (currentState : State) =
        match selfMessage with
        | OnError error ->
            JS.console.error error.Message
            currentState, Cmd.none

        | Timeout (id, appMsg) ->
            let remainingMessages =
                (Map.tryFind id currentState.PendingMessages |> Option.defaultValue 0) - 1

            if remainingMessages = 0 then
                { currentState with PendingMessages = Map.remove id currentState.PendingMessages }, Cmd.OfFunc.result appMsg
            else if remainingMessages > 0 then
                { currentState with PendingMessages = Map.add id remainingMessages currentState.PendingMessages }, Cmd.none
            else
                JS.console.warn "Invalid debouncer state: there was no state information for the supplier id"
                currentState, Cmd.none
