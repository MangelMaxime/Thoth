namespace Thoth.Elmish

module Debouncer =

    open System
    open Fable.PowerPack
    open Fable.Import
    open Elmish

    type Id = string

    type DebouncerState =
        { Delay : TimeSpan
          PendingMessages : Map<Id, int> }

    let create delay =
        { Delay = delay
          PendingMessages = Map.empty }

    type SelfMessage<'AppMsg> =
        | Timeout of id:Id * appMsg:'AppMsg
        | OnError of exn

    let bounce (id : Id) (msgToSend: 'a) (currentState : DebouncerState) =
        let counterInc =
            Option.map ((+) 1) >> Option.defaultValue 1

        let delayedCmd _ =
            promise {
                do! Promise.sleep (int currentState.Delay.TotalMilliseconds)
                return (id, msgToSend)
            }

        let updatedState =
            let newCounter = Map.tryFind id currentState.PendingMessages |> counterInc

            { currentState
                with PendingMessages = Map.add id newCounter currentState.PendingMessages }

        updatedState, Cmd.ofPromise delayedCmd () Timeout OnError

    let update (selfMessage : SelfMessage<_>) (currentState : DebouncerState) =
        match selfMessage with
        | OnError error ->
            Browser.console.error error.Message
            currentState, Cmd.none

        | Timeout (id, appMsg) ->
            let remainingMessages =
                (Map.tryFind id currentState.PendingMessages |> Option.defaultValue 0) - 1

            if remainingMessages = 0 then
                { currentState with PendingMessages = Map.remove id currentState.PendingMessages }, Cmd.ofMsg appMsg
            else if remainingMessages > 0 then
                { currentState with PendingMessages = Map.add id remainingMessages currentState.PendingMessages }, Cmd.none
            else
                Browser.console.warn "Invalid debouncer state: there was no state information for the supplier id"
                currentState, Cmd.none
