module Demos.FormBuilder

// This example has been inspired by:
// https://css-tricks.com/debouncing-throttling-explained-examples/


open Elmish
open Fable.Helpers.React
open Thoth.Elmish
open System
open Thoth.Elmish.FormBuilder
open Thoth.Elmish.FormBuilder.Fields
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
    | OnFormMsg of FormBuilder.Types.Msg

type Model =
    { FormState : FormBuilder.Types.Form<Msg> }

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

// TODO: This should be place has a default config in the library
let config : FormBuilder.Types.Config =
    Map.empty<FormBuilder.Types.FieldType, FormBuilder.Types.FieldConfig>
    |> Map.add "default-input" FormBuilder.Fields.Input.config
    |> Map.add "default-select" FormBuilder.Fields.Select.config
    |> Map.add "default-checkbox" FormBuilder.Fields.Checkbox.config
    |> Map.add "default-textarea" FormBuilder.Fields.Textarea.config
    |> Map.add "default-radio-button" FormBuilder.Fields.RadioButton.config

let form =
    Form.create OnFormMsg
    |> Form.addField
            ( Input.create "Firstname"
                |> Input.withDefaultRenderer )
    |> Form.addField
            ( Select.create "Favorite language"
                |> Select.withValues
                    [ "2", "F#"
                      "300", "Elm" ]
                |> Select.withDefaultRenderer )
    |> Form.addField
            ( Textarea.create "Description"
                |> Textarea.withDefaultRenderer )
    |> Form.addField
            ( Checkbox.create "I agree with the terms and conditions"
                |> Checkbox.withDefaultRenderer )
    |> Form.addField
            ( RadioButton.create "Make your profile public ?"
                |> RadioButton.withValues
                    [ "1", "Yes"
                      "2", "No" ]
                |> RadioButton.withDefaultRenderer )

let private init _ =
    let (formState, formCmds) = Form.init config form

    { FormState = formState }, Cmd.map OnFormMsg formCmds
    // { FormState = form }, Cmd.none

let private update msg model =
    match msg with
    | OnFormMsg msg ->
        let (formState, formCmd) = Form.update config msg model.FormState
        { model with FormState = formState }, Cmd.map OnFormMsg formCmd

    | Submit ->
        let (newForm, isValid) = Form.validate config model.FormState
        printfn "%b" isValid
        if isValid then
            printfn "%s" (Form.toJson config newForm)
            { model with FormState = Form.setWaiting true newForm }, Cmd.none
        else
            { model with FormState = newForm }, Cmd.none

open Fulma

let private formActions model dispatch =
    Field.div [ Field.IsGrouped
                Field.IsGroupedCentered ]
        [ Control.div [ ]
            [ Button.button [ Button.Color IsPrimary
                              Button.IsLoading (Form.isWaiting model.FormState)
                              Button.OnClick (fun _ ->
                                dispatch Submit
                              ) ]
                [ str "Submit" ] ]
          Control.div [ ]
            [ Button.button [ ]
                [ str "Reset" ] ] ]

let private view model dispatch =
    Columns.columns [ ]
        [ Column.column [ Column.Width(Screen.All, Column.Is6)
                          Column.Offset(Screen.All, Column.Is3) ]
            [ Form.render
                config
                model.FormState
                dispatch
                (formActions model dispatch) ] ]

open Elmish.React
open Elmish.Debug
open Elmish.HMR

let start (id : string) =
    Program.mkProgram init update view
    |> Program.withDebugger
    |> Program.withReactUnoptimized id
    |> Program.run
