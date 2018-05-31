module Tests.Decode

#if FABLE_COMPILER
open Fable.Core.JsInterop
open Thoth.Json.Decode
#else
open Thoth.Json.Net.Decode
#endif
open Util.Testing

type Record2 =
    { a : float
      b : float }

    static member Create a b =
        { a = a
          b = b }

type Record3 =
    { a : float
      b : float
      c : float }

    static member Create a b c =
        { a = a
          b = b
          c = c }

type Record4 =
    { a : float
      b : float
      c : float
      d : float }

    static member Create a b c d =
        { a = a
          b = b
          c = c
          d = d }

type Record5 =
    { a : float
      b : float
      c : float
      d : float
      e : float }

    static member Create a b c d e =
        { a = a
          b = b
          c = c
          d = d
          e = e }

type Record6 =
    { a : float
      b : float
      c : float
      d : float
      e : float
      f : float }

    static member Create a b c d e f =
        { a = a
          b = b
          c = c
          d = d
          e = e
          f = f }

type Record7 =
    { a : float
      b : float
      c : float
      d : float
      e : float
      f : float
      g : float }

    static member Create a b c d e f g =
        { a = a
          b = b
          c = c
          d = d
          e = e
          f = f
          g = g }

type Record8 =
    { a : float
      b : float
      c : float
      d : float
      e : float
      f : float
      g : float
      h : float }

    static member Create a b c d e f g h =
        { a = a
          b = b
          c = c
          d = d
          e = e
          f = f
          g = g
          h = h }

type MyUnion = Foo of int

type Record9 =
    { a: int
      b: string
      c: bool list
      d: MyUnion
      e: Map<string, Record2>
    }

type User =
    { Id : int
      Name : string
      Email : string
      Followers : int }

    static member Create id email name followers =
        { Id = id
          Name = name
          Email = email
          Followers = followers }

let jsonRecord =
    """{ "a": 1.0,
         "b": 2.0,
         "c": 3.0,
         "d": 4.0,
         "e": 5.0,
         "f": 6.0,
         "g": 7.0,
         "h": 8.0 }"""

let jsonRecordInvalid =
    """{ "a": "invalid_a_field",
         "b": "invalid_a_field",
         "c": "invalid_a_field",
         "d": "invalid_a_field",
         "e": "invalid_a_field",
         "f": "invalid_a_field",
         "g": "invalid_a_field",
         "h": "invalid_a_field" }"""

let tests : Test =
    testList "Thoth.Json.Decode" [

        #if FABLE_COMPILER

        testList "Errors" [

            testCase "circular structure are supported when reporting error" <| fun _ ->
                let a = createObj [ ]
                let b = createObj [ ]
                a?child <- b
                b?child <- a

                let expected : Result<float, string>= Error "Expecting a float but decoder failed. Couldn\'t report given value due to circular structure. "
                let actual = decodeValue float b

                equal expected actual

        ]

        #endif

        testList "Primitives" [

            testCase "a string works" <| fun _ ->
                let expected = Ok("maxime")
                let actual =
                    decodeString string "\"maxime\""

                equal expected actual

            testCase "a float works" <| fun _ ->
                let expected = Ok(1.2)
                let actual =
                    decodeString float "1.2"

                equal expected actual

            testCase "a float from int works" <| fun _ ->
                let expected = Ok(1.0)
                let actual =
                    decodeString float "1"

                equal expected actual

            testCase "a bool works" <| fun _ ->
                let expected = Ok(true)
                let actual =
                    decodeString bool "true"

                equal expected actual

            testCase "an invalid bool output an error" <| fun _ ->
                let expected = Error("Expecting a boolean but instead got: 2")
                let actual =
                    decodeString bool "2"

                equal expected actual

            testCase "an int works" <| fun _ ->
                let expected = Ok(25)
                let actual =
                    decodeString int "25"

                equal expected actual

            testCase "an invalid int [invalid range: too big] output an error" <| fun _ ->
                let expected = Error("Expecting an int but instead got: 2147483648\nReason: Value was either too large or too small for an int")
                let actual =
                    decodeString int "2147483648"

                equal expected actual

            testCase "an invalid int [invalid range: too small] output an error" <| fun _ ->
                let expected = Error("Expecting an int but instead got: -2147483649\nReason: Value was either too large or too small for an int")
                let actual =
                    decodeString int "-2147483649"

                equal expected actual
        ]

        testList "Object primitives" [

            testCase "field works" <| fun _ ->
                let json = """{ "name": "maxime", "age": 25 }"""
                let expected = Ok("maxime")

                let actual =
                    decodeString (field "name" string) json

                equal expected actual

            testCase "field output an error when field is missing" <| fun _ ->
                let json = """{ "name": "maxime", "age": 25 }"""
                let expected =
                    Error(
                        """
Expecting an object with a field named `height` but instead got:
{
    "name": "maxime",
    "age": 25
}
                        """.Trim())

                let actual =
                    decodeString (field "height" float) json

                equal expected actual

            testCase "at works" <| fun _ ->

                let json = """{ "user": { "name": "maxime", "age": 25 } }"""
                let expected = Ok "maxime"

                let actual =
                    decodeString (at ["user"; "name"] string) json

                equal expected actual

            testCase "at output an error if the path failed" <| fun _ ->
                let json = """{ "user": { "name": "maxime", "age": 25 } }"""
                let expected =
                    Error(
                        """
Expecting an object with path `user.firstname` but instead got:
{
    "user": {
        "name": "maxime",
        "age": 25
    }
}
Node `firstname` is unkown.
                        """.Trim())

                let actual =
                    decodeString (at ["user"; "firstname"] string) json

                equal expected actual

            testCase "index works" <| fun _ ->
                let json = """["maxime", "alfonso", "steffen"]"""
                let expected = Ok("alfonso")

                let actual =
                    decodeString (index 1 string) json

                equal expected actual

            testCase "index output an error if array is to small" <| fun _ ->
                let json = """["maxime", "alfonso", "steffen"]"""
                let expected =
                    Error(
                        """
Expecting a longer array. Need index `5` but there are only `3` entries.
[
    "maxime",
    "alfonso",
    "steffen"
]
                        """.Trim())

                let actual =
                    decodeString (index 5 string) json

                equal expected actual

            testCase "index output an error if value isn't an array" <| fun _ ->
                let json = "1"
                let expected =
                    Error(
                        """
Expecting an array but instead got: 1
                        """.Trim())

                let actual =
                    decodeString (index 5 string) json

                equal expected actual

        ]


        testList "Data structure" [

            testCase "list works" <| fun _ ->
                let expected = Ok([1; 2; 3])

                let actual =
                    decodeString (list int) "[1, 2, 3]"

                equal expected actual

            testCase "an invalid list output an error" <| fun _ ->
                let expected = Error("Expecting a list but instead got: 1")

                let actual =
                    decodeString (list int) "1"

                equal expected actual

            testCase "array works" <| fun _ ->
                // Need to pass by a list otherwise Fable use:
                // new Int32Array([1, 2, 3]) and the test fails
                // And this would give:
                // Expected: Result { tag: 0, data: Int32Array [ 1, 2, 3 ] }
                // Actual: Result { tag: 0, data: [ 1, 2, 3 ] }
                let expected = Ok([1; 2; 3] |> List.toArray)

                let actual =
                    decodeString (array int) "[1, 2, 3]"

                equal expected actual

            testCase "an invalid array output an error" <| fun _ ->
                let expected = Error("Expecting an array but instead got: 1")

                let actual =
                    decodeString (array int) "1"

                equal expected actual

            testCase "keyValuePairs works" <| fun _ ->
                let expected = Ok([("a", 1) ; ("b", 2) ; ("c", 3)])

                let actual =
                    decodeString (keyValuePairs int) """{ "a": 1, "b": 2, "c": 3 }"""

                equal expected actual

            testCase "dict works" <| fun _ ->
                let expected = Ok(Map.ofList([("a", 1) ; ("b", 2) ; ("c", 3)]))

                let actual =
                    decodeString (dict int) """{ "a": 1, "b": 2, "c": 3 }"""

                equal expected actual

            testCase "dict with custom decoder works" <| fun _ ->
                let expected = Ok(Map.ofList([("a", Record2.Create 1. 1.) ; ("b", Record2.Create 2. 2.) ; ("c", Record2.Create 3. 3.)]))

                let decodePoint =
                    map2 Record2.Create
                        (field "a" float)
                        (field "b" float)

                let actual =
                    decodeString (dict decodePoint)
                        """
{
    "a":
        {
            "a": 1.0,
            "b": 1.0
        },
    "b":
        {
            "a": 2.0,
            "b": 2.0
        },
    "c":
        {
            "a": 3.0,
            "b": 3.0
        }
}
                        """

                equal expected actual

            testCase "an invalid dict output an error" <| fun _ ->
                let expected = Error("Expecting an object but instead got: 1")

                let actual =
                    decodeString (dict int) "1"

                equal expected actual

        ]

        testList "Inconsistent structure" [

            testCase "oneOf works" <| fun _ ->
                let expected = Ok([1; 2; 0; 4])

                let badInt =
                    oneOf [ int; nil 0 ]

                let actual =
                    decodeString (list badInt) "[1,2,null,4]"

                equal expected actual


            testCase "oneOf output errors if all case fails" <| fun _ ->
                let expected =
                    Error (
                        """
I run into the following problems:

Expecting a string but instead got: 1
Expecting an object with a field named `test` but instead got:
1
                        """.Trim())

                let badInt =
                    oneOf [ string; field "test" string ]

                let actual =
                    decodeString (list badInt) "[1,2,null,4]"

                equal expected actual

            testCase "optional works" <| fun _ ->
                let json = """{ "name": "maxime", "age": 25 }"""

                let expectedValid = Ok(Some "maxime")
                let actualValid =
                    decodeString (option (field "name" string) ) json

                equal expectedValid actualValid

                let expectedInvalidType = Ok(None)
                let actualInvalidType =
                    decodeString (option (field "name" int) ) json

                equal expectedInvalidType actualInvalidType

                let expectedMissingField = Ok(None)
                let actualMissingField =
                    decodeString (option (field "height" int) ) json

                equal expectedMissingField actualMissingField

        ]

        testList "Fancy decoding" [

            testCase "null works (test on an int)" <| fun _ ->
                let expected = Ok(20)
                let actual =
                    decodeString (nil 20) "null"

                equal expected actual

            testCase "null works (test on a boolean)" <| fun _ ->
                let expected = Ok(false)
                let actual =
                    decodeString (nil false) "null"

                equal expected actual

            testCase "succeed works" <| fun _ ->
                let expected = Ok(7)
                let actual =
                    decodeString (succeed 7) "true"

                equal expected actual

            testCase "succeed output an error if the JSON is invalid" <| fun _ ->
                #if FABLE_COMPILER
                let expected = Error("Given an invalid JSON: Unexpected token m in JSON at position 0")
                #else
                let expected = Error("Given an invalid JSON: Unexpected character encountered while parsing value: m. Path '', line 0, position 0.")
                #endif
                let actual =
                    decodeString (succeed 7) "maxime"

                equal expected actual

            testCase "fail works" <| fun _ ->
                let msg = "Failing because it's fun"
                let expected = Error("I run into a `fail` decoder: " + msg)
                let actual =
                    decodeString (fail msg) "true"

                equal expected actual

            testCase "andThen works" <| fun _ ->
                let expected = Ok 1
                let infoHelp version =
                    match version with
                    | 4 ->
                        succeed 1
                    | 3 ->
                        succeed 1
                    | _ ->
                        fail <| "Tying to decode info, but version " + (version.ToString()) + "is not supported"

                let info =
                    field "version" int
                    |> andThen infoHelp

                let actual =
                    decodeString info """{ "version": 3, "data": 2 }"""

                equal expected actual


            testCase "andThen generate an error if an error occuered" <| fun _ ->
                let expected =
                    Error(
                        """
Expecting an object with a field named `version` but instead got:
{
    "info": 3,
    "data": 2
}
                        """.Trim())
                let infoHelp version =
                    match version with
                    | 4 ->
                        succeed 1
                    | 3 ->
                        succeed 1
                    | _ ->
                        fail <| "Tying to decode info, but version " + (version.ToString()) + "is not supported"

                let info =
                    field "version" int
                    |> andThen infoHelp

                let actual =
                    decodeString info """{ "info": 3, "data": 2 }"""

                equal expected actual

        ]

        testList "Mapping" [

            testCase "map works" <| fun _ ->
                let expected = Ok(6)
                let stringLength =
                    map String.length string

                let actual =
                    decodeString stringLength "\"maxime\""
                equal expected actual


            testCase "map2 works" <| fun _ ->
                let expected = Ok({a = 1.; b = 2.} : Record2)

                let decodePoint =
                    map2 Record2.Create
                        (field "a" float)
                        (field "b" float)

                let actual =
                    decodeString decodePoint jsonRecord

                equal expected actual

            testCase "map3 works" <| fun _ ->
                let expected = Ok({ a = 1.
                                    b = 2.
                                    c = 3. } : Record3)

                let decodePoint =
                    map3 Record3.Create
                        (field "a" float)
                        (field "b" float)
                        (field "c" float)

                let actual =
                    decodeString decodePoint jsonRecord

                equal expected actual

            testCase "map4 works" <| fun _ ->
                let expected = Ok({ a = 1.
                                    b = 2.
                                    c = 3.
                                    d = 4. } : Record4)

                let decodePoint =
                    map4 Record4.Create
                        (field "a" float)
                        (field "b" float)
                        (field "c" float)
                        (field "d" float)

                let actual =
                    decodeString decodePoint jsonRecord

                equal expected actual

            testCase "map5 works" <| fun _ ->
                let expected = Ok({ a = 1.
                                    b = 2.
                                    c = 3.
                                    d = 4.
                                    e = 5. } : Record5)

                let decodePoint =
                    map5 Record5.Create
                        (field "a" float)
                        (field "b" float)
                        (field "c" float)
                        (field "d" float)
                        (field "e" float)

                let actual =
                    decodeString decodePoint jsonRecord

                equal expected actual

            testCase "map6 works" <| fun _ ->
                let expected = Ok({ a = 1.
                                    b = 2.
                                    c = 3.
                                    d = 4.
                                    e = 5.
                                    f = 6. } : Record6)

                let decodePoint =
                    map6 Record6.Create
                        (field "a" float)
                        (field "b" float)
                        (field "c" float)
                        (field "d" float)
                        (field "e" float)
                        (field "f" float)

                let actual =
                    decodeString decodePoint jsonRecord

                equal expected actual

            testCase "map7 works" <| fun _ ->
                let expected = Ok({ a = 1.
                                    b = 2.
                                    c = 3.
                                    d = 4.
                                    e = 5.
                                    f = 6.
                                    g = 7. } : Record7)

                let decodePoint =
                    map7 Record7.Create
                        (field "a" float)
                        (field "b" float)
                        (field "c" float)
                        (field "d" float)
                        (field "e" float)
                        (field "f" float)
                        (field "g" float)

                let actual =
                    decodeString decodePoint jsonRecord

                equal expected actual

            testCase "map8 works" <| fun _ ->
                let expected = Ok({ a = 1.
                                    b = 2.
                                    c = 3.
                                    d = 4.
                                    e = 5.
                                    f = 6.
                                    g = 7.
                                    h = 8. } : Record8)

                let decodePoint =
                    map8 Record8.Create
                        (field "a" float)
                        (field "b" float)
                        (field "c" float)
                        (field "d" float)
                        (field "e" float)
                        (field "f" float)
                        (field "g" float)
                        (field "h" float)

                let actual =
                    decodeString decodePoint jsonRecord

                equal expected actual

            testCase "map2 generate an error if invalid" <| fun _ ->
                let expected = Error("Expecting a float but instead got: \"invalid_a_field\"")

                let decodePoint =
                    map2 Record2.Create
                        (field "a" float)
                        (field "b" float)

                let actual =
                    decodeString decodePoint jsonRecordInvalid

                equal expected actual

        ]

        testList "Pipeline" [

            testCase "required works" <| fun _ ->
                let expected =
                    Ok(User.Create 67 "user@mail.com" "" 0)

                let userDecoder =
                    decode User.Create
                        |> required "id" int
                        |> required "email" string
                        |> optional "name" string ""
                        |> hardcoded 0

                let actual =
                    decodeString
                        userDecoder
                        """{ "id": 67, "email": "user@mail.com" }"""

                equal expected actual

        ]

        #if FABLE_COMPILER
        testList "Auto" [
            testCase "Auto.Generate works" <| fun _ ->
                let r1 =
                    { a = 5
                      b = "bar"
                      c = [false; true; false]
                      d = Foo 14
                      e = Map [("oh", { a = 2.; b = 2. }); ("ah", { a = -1.5; b = 0. })]
                    }
                let json = Thoth.Json.Encode.encodeAuto 4 r1
                printfn "AUTO ENCODED: %s" json
                let decoder = Thoth.Json.Decode.Auto.Generate<Record9>()
                match decodeString decoder json with
                | Error er -> failwith er
                | Ok r2 ->
                    equal 5 r2.a
                    equal "bar" r2.b
                    equal [false; true; false] r2.c
                    equal (Foo 14) r2.d
                    equal -1.5 (Map.find "ah" r2.e).a
                    equal 2.   (Map.find "oh" r2.e).b
        ]
        #endif
    ]
