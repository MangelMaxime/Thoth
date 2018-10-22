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

type Form.ErrorDef with
    static member Decoder =
        Decode.object
            (fun get ->
                { Text = get.Required.Field "text" Decode.string
                  Key = get.Required.Field "key" Decode.string } : Form.ErrorDef )

    static member Encoder (error : Form.ErrorDef) =
        Encode.object [
            "text", Encode.string error.Text
            "key", Encode.string error.Key
        ]

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

    let checkForErrors (body : string) =
        promise {
            do! Promise.sleep 300
            match Decode.fromString FormData.Decoder body with
            | Ok formData ->
                return
                    [ if formData.Email = "mangel.maxime@mail.com" then
                        yield ({ Key = "email"
                                 Text = "This email is already used" } : Form.ErrorDef)
                      if formData.Firstname.ToLower() = "Maxime".ToLower()
                            && formData.Surname.ToLower() = "Mangel".ToLower() then
                        let msg = sprintf """A user "%s %s" already exist""" (capitalize formData.Firstname) (capitalize formData.Surname)
                        yield ({ Key = "global_error"
                                 Text = msg } : Form.ErrorDef)
                    ]
                    |> List.map Form.ErrorDef.Encoder
                    |> Encode.list
                    |> Encode.toString 0
            | Error msg ->
                return failwith msg
        }

type Msg =
    | Submit
    | OnFormMsg of Form.Msg
    | ChangeFirstname of string

type Model =
    { FormState : Form.FormState<Msg>
      Firstname : string
      FirstnameError : string }

let getJobsList =
    promise {
        let! res = FakeServer.getJobs ()
        return res
    }

let validateFromServer (body : string) =
    promise {
        let! res = FakeServer.checkForErrors body
        match Decode.fromString (Decode.list Form.ErrorDef.Decoder) res with
        | Ok errors ->
            return errors
        | Error msg ->
            return failwith msg
    }

let private createForm =
    let age =
        input {
            label "Age"
            isRequired
            placeholder "Ex: 18"
        }

    let firstname = input {
        label "Firstname"
        jsonLabel "firstname"
        isRequired
        placeholder "Ex: Maxime"
    }

    let surname = input {
        label "Surname"
        jsonLabel "surname"
        isRequired
        placeholder "Ex: Mangel"
    }

    let email = input {
        label "Email"
        jsonLabel "email"
        isRequired
        placeholder "Ex: mangel.maxime@mail.com"
    }

    let domains = select {
        label "Favorite language"
        jsonLabel "favLang"
        isRequired
        valuesFromServer getJobsList
    }

    let submit = button {
        label "Create"
        isPrimary
        onClick Submit
    }

    let cancel = button {
        label "Cancel"
    }

    form {
        onChange OnFormMsg
        serverValidation validateFromServer

        addInput firstname
        addInput surname
        addInput age
        addInput email
        addSelect domains

        addAction submit
        addAction cancel
    }

let private init _ =
    let (formState, formCmds) = createForm |> Form.init

    { FormState = formState
      Firstname = ""
      FirstnameError = "" }, Cmd.map OnFormMsg formCmds

let private update msg model =
    match msg with
    | OnFormMsg msg ->
        let (formState, formCmd) = Form.update msg model.FormState
        { model with FormState = formState }, Cmd.map OnFormMsg formCmd

    | Submit ->
        // if model.IsWaitingServer then
        //     model, Cmd.none
        // else
        let (newForm, isValid) = Form.validate model.FormState
        if isValid then
            printfn "%s" (Form.toJson newForm)
            { model with FormState = Form.setWaiting true newForm }, Cmd.none
        else
            { model with FormState = newForm }, Cmd.none

    | ChangeFirstname newValue ->
        if newValue <> "" then
            { model with Firstname = newValue
                         FirstnameError = "" }, Cmd.none
        else
            { model with Firstname = newValue
                         FirstnameError = "This field is required" }, Cmd.none

let private view model dispatch =
    Columns.columns [ ]
        [ Column.column [ Column.Width(Screen.All, Column.Is6)
                          Column.Offset(Screen.All, Column.Is3) ]
            [ Form.render model.FormState dispatch
            //   Field.div [ ]
            //     [ Label.label [ ]
            //         [ str "Firstname" ]
            //       Control.div [ ]
            //         [ Input.input [ Input.Value model.Firstname
            //                         Input.Placeholder "Ex: Maxime"
            //                         Input.OnChange (fun ev ->
            //                             ev.Value |> string |> ChangeFirstname |> dispatch
            //                         ) ] ]
            //       Help.help [ Help.Color IsDanger ]
            //         [ str model.FirstnameError ] ]
              // ...
            ] ]


open Elmish.React
open Elmish.Debug
open Elmish.HMR

let start (id : string) =
    Program.mkProgram init update view
    |> Program.withReactUnoptimized id
    |> Program.run


// module Test =
//     open System

//     type ButtonState<'T> =
//         class end

//     type ValidationState =
//             | Valid
//             | Invalid of string

//     type InputState =
//         { Label : string
//           Placeholder : string option
//           Value : string
//           Validators : InputValidator list
//           ValidationInputState : ValidationState }

//     and InputValidator = InputState -> ValidationState

//     type FormState<'AppMsg> =
//         { Fields : (Guid * InputState) list
//           Actions : ButtonState<'AppMsg> list }

//     let form =
//         { Fields =
//             [ (Guid.NewGuid(), { Label = "Firstname"
//                                  Placeholder = None
//                                  Value = ""
//                                  Validators = []
//                                  ValidationInputState = Valid })
//               (Guid.NewGuid(), { Label = "Lastname"
//                                  Placeholder = Some "Ex: Mangel"
//                                  Value = ""
//                                  Validators = [ isRequired ]
//                                  ValidationInputState = Valid }) ]
//           Actions = [ ]
//         }

// let private createForm =
//     let firstname = input {
//         label "Firstname"
//         placeholder "Ex: Maxime"
//         isRequired
//     }

//     let surname = input {
//         label "Surname"
//         placeholder "Ex: Mangel"
//         isRequired
//     }

//     form {
//         addInput firstname
//         addInput surname
//     }

// let private createForm =
//     Form.create ()
//     |> Form.addInput
//         ( Form.Input.create ()
//             |> Form.Input.label "Firstname"
//             |> Form.Input.placeholder "Ex: Maxime"
//             |> Form.Input.isRequired )
//     |> Form.addInput
//         ( Form.Input.create ()
//             |> Form.Input.label "Surname"
//             |> Form.Input.placeholder "Ex: Mangel"
//             |> Form.Input.isRequired )

module Test2 =

    open Fable.Import

    type Key = string

    type SelectState =
        { Label : string
          SelectedKey : Key option
          Values : (Key * string) list
          Placeholder : (Key * string) option
          Validators : SelectValidator list
          ValidationSelectState : ValidationState
          IsLoading : bool
          ValuesFromServer : JS.Promise<(Key * string) list> option }

    and SelectValidator = SelectState -> ValidationState
