module Tests.Encode

open Fable.Core
open Thot.Json.Encode
open Fable.Core.Testing

[<Global>]
let it (msg: string) (f: unit->unit): unit = jsNative

[<Global>]
let describe (msg: string) (f: unit->unit): unit = jsNative

describe "Encode" <| fun _ ->

    it "a string works" <| fun _ ->
        let expected = "\"maxime\""
        let actual =
            string "maxime"
            |> encode 0
        Assert.AreEqual(expected, actual)

    it "an int works" <| fun _ ->
        let expected = "1"
        let actual =
            int 1
            |> encode 0
        Assert.AreEqual(expected, actual)

    it "a float works" <| fun _ ->
        let expected = "1.2"
        let actual =
            float 1.2
            |> encode 0
        Assert.AreEqual(expected, actual)

    it "an array works" <| fun _ ->
        let expected =
            """["maxime",2]"""
        let actual =
            array
                [| string "maxime"
                   int 2
                |] |> encode 0
        Assert.AreEqual(expected, actual)

    it "a list works" <| fun _ ->
        let expected =
            """["maxime",2]"""
        let actual =
            list
                [ string "maxime"
                  int 2
                ] |> encode 0
        Assert.AreEqual(expected, actual)

    it "a bool works" <| fun _ ->
        let expected = "false"
        let actual =
            bool false
            |> encode 0
        Assert.AreEqual(expected, actual)

    it "a null works" <| fun _ ->
        let expected = "null"
        let actual =
            nil
            |> encode 0
        Assert.AreEqual(expected, actual)

    it "an object works" <| fun _ ->
        let expected =
            """{"firstname":"maxime","age":25}"""
        let actual =
            object
                [ ("firstname", string "maxime")
                  ("age", int 25)
                ] |> encode 0
        Assert.AreEqual(expected, actual)

    it "a dict works" <| fun _ ->
        let expected =
            """{"a":1,"b":2,"c":3}"""
        let actual =
            Map.ofList
                [ ("a", int 1)
                  ("b", int 2)
                  ("c", int 3)
                ]
            |> dict
            |> encode 0
        Assert.AreEqual(expected, actual)

    it "using pretty space works" <| fun _ ->
        let expected = "{\n    \"firstname\": \"maxime\",\n    \"age\": 25\n}"

        let actual =
            object
                [ ("firstname", string "maxime")
                  ("age", int 25)
                ] |> encode 4
        Assert.AreEqual(expected, actual)

    it "complexe structure works" <| fun _ ->
        let expected =
            "{\n    \"firstname\": \"maxime\",\n    \"age\": 25,\n    \"address\": {\n        \"street\": \"main road\",\n        \"city\": \"Bordeaux\"\n    }\n}"

        let actual =
            object
                [ ("firstname", string "maxime")
                  ("age", int 25)
                  ("address", object
                                [ "street", string "main road"
                                  "city", string "Bordeaux"
                                ])
                ] |> encode 4
        Assert.AreEqual(expected, actual)
