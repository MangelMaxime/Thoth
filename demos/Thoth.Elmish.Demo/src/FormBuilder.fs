module Demos.FormBuilder

// This example has been inspired by:
// https://css-tricks.com/debouncing-throttling-explained-examples/


open Elmish
open Fable.Helpers.React
open Fulma
open Thoth.Elmish
open System
open Thoth.Elmish.FormBuilder
open Fable.PowerPack
open Thoth.Json

// type Form.ErrorDef with
//     static member Decoder =
//         Decode.object
//             (fun get ->
//                 { Text = get.Required.Field "text" Decode.string
//                   Key = get.Required.Field "key" Decode.string } : Form.ErrorDef )

//     static member Encoder (error : Form.ErrorDef) =
//         Encode.object [
//             "text", Encode.string error.Text
//             "key", Encode.string error.Key
//         ]

module FakeServer =

    let decodeIntFromString =
        Decode.string
        |> Decode.map int

    type FormData =
        { Firstname : string
          Surname : string
          Email : string
          FavLang : int }

        static member Decoder =
            Decode.object (fun get ->
                { Firstname = get.Required.Field "firstname" Decode.string
                  Surname = get.Required.Field "surname" Decode.string
                  Email = get.Required.Field "email" Decode.string
                  FavLang = get.Required.Field "favLang" decodeIntFromString } : FormData
            )

    let getJobs () =
        promise {
            do! Promise.sleep 5000
            return [
                "1", "C"
                "10", "C#"
                "4", "Clojure"
                "7", "Elm"
                "9", "F#"
                "269", "JavaScript"
            ]
        }

    let capitalize (text : string) =
        let firstLetter = Char.ToUpper text.[0]
        string firstLetter + text.ToLower().[1..text.Length]

    // let checkForErrors (body : string) =
    //     promise {
    //         do! Promise.sleep 300
    //         match Decode.fromString FormData.Decoder body with
    //         | Ok formData ->
    //             return
    //                 [ if formData.Email = "mangel.maxime@mail.com" then
    //                     yield ({ Key = "email"
    //                              Text = "This email is already used" } : Form.ErrorDef)
    //                   if formData.Firstname.ToLower() = "Maxime".ToLower()
    //                         && formData.Surname.ToLower() = "Mangel".ToLower() then
    //                     let msg = sprintf """A user "%s %s" already exist""" (capitalize formData.Firstname) (capitalize formData.Surname)
    //                     yield ({ Key = "global_error"
    //                              Text = msg } : Form.ErrorDef)
    //                 ]
    //                 |> List.map Form.ErrorDef.Encoder
    //                 |> Encode.list
    //                 |> Encode.toString 0
    //         | Error msg ->
    //             return failwith msg
    //     }

type Msg =
    | Submit
    | OnFormMsg of Form.Msg

type Model =
    { FormState : Form.Form<Msg> }

let getJobsList =
    promise {
        let! res = FakeServer.getJobs ()
        return res
    }

// let validateFromServer (body : string) =
//     promise {
//         let! res = FakeServer.checkForErrors body
//         match Decode.fromString (Decode.list Form.ErrorDef.Decoder) res with
//         | Ok errors ->
//             return errors
//         | Error msg ->
//             return failwith msg
//     }

// let private createForm =
//     let firstname =
//         input {
//             label "Firstname"
//             placeholder "Ex: Maxime"
//             jsonLabel "firstname"
//             isRequired
//         }

//     let surname =
//         input {
//             label "surname"
//             jsonLabel "surname"
//             placeholder "Ex: Mangel"
//             isRequired
//         }

//     let favoriteLanguage =
//         select {
//             label "Favorite language"
//             jsonLabel "favLanguage"
//             isRequired
//             valuesFromServer getJobsList
//         }

//     let submit =
//         button {
//             label "Submit"
//             onClick Submit
//             isPrimary
//         }

//     form {
//         onChange OnFormMsg

//         addInput firstname
//         addInput surname
//         addSelect favoriteLanguage

//         addAction submit
//     }

let form =
    Form.create OnFormMsg
    |> Form.addField { Type = Form.Input
                       State = { Form.InputState.Empty with Label = "Firstname" }
                       Id = "firstname" }
    |> Form.addField { Type = Form.Input
                       State = { Form.InputState.Empty with Label = "Surname" }
                       Id = "surname" }
    |> Form.addField { Type = Form.Select
                       State = { Form.SelectState.Empty
                                    with Label = "Favorite Language"
                                         Values = [ ]
                                         ValuesFromServer = Some getJobsList }
                       Id = "favLang" }

let private init _ =
    let (formState, formCmds) = Form.init form

    { FormState = formState }, Cmd.map OnFormMsg formCmds
    // { FormState = form }, Cmd.none

let private update msg model =
    match msg with
    | OnFormMsg msg ->
        let (formState, formCmd) = Form.update msg model.FormState
        { model with FormState = formState }, Cmd.map OnFormMsg formCmd

    // | Submit ->
    //     let (newForm, isValid) = Form.validate model.FormState
    //     if isValid then
    //         printfn "%s" (Form.toJson newForm)
    //         { model with FormState = Form.setWaiting true newForm }, Cmd.none
    //     else
    //         { model with FormState = newForm }, Cmd.none

let private view model dispatch =
    Columns.columns [ ]
        [ Column.column [ Column.Width(Screen.All, Column.Is6)
                          Column.Offset(Screen.All, Column.Is3) ]
            [ Form.render model.FormState dispatch ] ]

open Elmish.React
open Elmish.Debug
open Elmish.HMR

let start (id : string) =
    Program.mkProgram init update view
    |> Program.withDebugger
    |> Program.withReactUnoptimized id
    |> Program.run
