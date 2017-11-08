module Tests.Decode

open Fable.Core
open Fable.Core.JsInterop
open Fable.Json
open Fable.Core.Testing

[<Global>]
let it (msg: string) (f: unit->unit): unit = jsNative

// it "Decoding a valid string value works" <| fun () ->
//     let expected = "maxime"
//     let actual =
//         Decode.string "maxime"
//         |> Decode.decode
//     Assert.AreEqual(expected, actual)

// it "Decoding an invalid value fail" <| fun () ->
//     let expected = Error(Decode.makeErr "Must be a string" 2 [])
//     let actual =
//         Decode.string 2
//         |> Decode.decode
//     Assert.AreEqual(expected, actual)
