module Tests.Decode

open Fable.Core
open Fable.Core.JsInterop
open Fable.Json.Decode
open Fable.Core.Testing

[<Global>]
let it (msg: string) (f: unit->unit): unit = jsNative

it "Adding works" <| fun () ->
    let expected = 3
    let actual = add 1 2
    Assert.AreEqual(expected,actual)

it "Adding works3" <| fun () ->
    let expected = 3
    let actual = add 1 2
    Assert.AreEqual(expected,actual)
