module Demos.Debouncer

// This example has been inspired by:
// https://css-tricks.com/debouncing-throttling-explained-examples/

open Elmish
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fulma
open Thoth.Elmish
open System

type Model =
    { Debouncer : Debouncer.State
      UserInput : string
      ShowMessage : bool
      IsTyping : bool  }

type Msg =
    | DebouncerSelfMsg of Debouncer.SelfMessage<Msg>
    | ChangeValue of string
    | EndOfInput
    | Reset

let private init _ =
    { Debouncer = Debouncer.create()
      UserInput = ""
      ShowMessage = false
      IsTyping = false }, Cmd.none

let private update msg model =
    match msg with
    | ChangeValue newValue ->
        let (debouncerModel, debouncerCmd) =
            model.Debouncer
            |> Debouncer.bounce (TimeSpan.FromSeconds 1.5) "user_input" EndOfInput

        { model with UserInput = newValue
                     IsTyping = true
                     Debouncer = debouncerModel }, Cmd.batch [ Cmd.map DebouncerSelfMsg debouncerCmd ]

    | DebouncerSelfMsg debouncerMsg ->
        let (debouncerModel, debouncerCmd) = Debouncer.update debouncerMsg model.Debouncer
        { model with Debouncer = debouncerModel }, debouncerCmd

    | EndOfInput ->
        let (debouncerModel, debouncerCmd) =
            model.Debouncer
            |> Debouncer.bounce (TimeSpan.FromSeconds 3.) "reset_demo" Reset

        { model with ShowMessage = true
                     Debouncer = debouncerModel }, Cmd.batch [ Cmd.map DebouncerSelfMsg debouncerCmd ]

    | Reset ->
        { model with UserInput = ""
                     IsTyping = false
                     ShowMessage = false }, Cmd.none

let private view model dispatch =
    let instruction =
        if model.IsTyping then
            str "Waiting for more keystrokes... "
          else
            str "Type here, I will detect when you stop typing"

    let message =
        if model.ShowMessage then
            str "You stop typing. Making now the request" |> Some
        else
            None

    div [ ]
        [ instruction
          Input.text [ Input.OnChange (fun ev -> dispatch (ChangeValue ev.Value))
                       Input.Value model.UserInput ]
          ofOption message ]

open Elmish.React
open Elmish.Debug
open Elmish.HMR


let start (id : string) =
    Program.mkProgram init update view
    |> Program.withReactUnoptimized id
    |> Program.run
