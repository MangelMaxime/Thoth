module Tests.Decode

open Fable.Core
open Fable.Core.JsInterop
open Fable.Core.Testing
open Thot.Json.Decode

[<Global>]
let it (msg: string) (f: unit->unit): unit = jsNative


[<Global>]
let describe (msg: string) (f: unit->unit): unit = jsNative

type Point =
    { x : float
      y : float }

    static member Create x y =
        { x = x
          y = y }

describe "Decode" <| fun _ ->

    it "a string works" <| fun _ ->
        let expected = Ok("maxime")
        let actual =
            decodeString string "\"maxime\""

        // Fable.Import.JS.console.log (Ok("maxime"))
        // Fable.Import.JS.console.log (Error("maxime"))

        Assert.AreEqual(expected, actual)

    it "a float works" <| fun _ ->
        let expected = Ok(1.2)
        let actual =
            decodeString float "1.2"

        // Fable.Import.JS.console.log (Ok("maxime"))
        // Fable.Import.JS.console.log (Error("maxime"))

        Assert.AreEqual(expected, actual)

    it "an object works" <| fun _ ->
        let expected = Ok({x = 3.; y = 4.} : Point)

        let decodePoint =
            map2 Point.Create
                (field "x" float)
                (field "y" float)

        let actual =
            decodeString decodePoint """{ "x": 3, "y": 4 }"""

        Assert.AreEqual(expected, actual)
