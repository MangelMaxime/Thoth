module Tests.Decode

#if FABLE_COMPILER
open Fable.Import
open Fable.Core
open Fable.Core.JsInterop
open Thoth.Json
#else
open Thoth.Json.Net
#endif
open Util.Testing
open System

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
      c: (bool * int) list
      d: (MyUnion option) []
      e: Map<string, Record2>
      f: System.DateTime
    }

type User =
    { Id : int
      Name : string
      Email : string
      Followers : int }

    static member Create id name email followers =
        { Id = id
          Name = name
          Email = email
          Followers = followers }

type SmallRecord =
    { fieldA: string }

type SmallRecord2 =
    { optionalField : string option }

type MyList<'T> =
    | Nil
    | Cons of 'T * MyList<'T>

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

        testList "Errors" [

            #if FABLE_COMPILER

            testCase "circular structure are supported when reporting error" <| fun _ ->
                let a = createObj [ ]
                let b = createObj [ ]
                a?child <- b
                b?child <- a

                let expected : Result<float, string>= Error "Error at: `$`\nExpecting a float but decoder failed. Couldn\'t report given value due to circular structure. "
                let actual = Decode.fromValue "$" Decode.float b

                equal expected actual
            #endif

            testCase "invalid json" <| fun _ ->
                #if FABLE_COMPILER
                let expected : Result<float, string>= Error "Given an invalid JSON: Unexpected token m in JSON at position 0"
                #else
                let expected : Result<float, string>= Error "Given an invalid JSON: Unexpected character encountered while parsing value: m. Path '', line 0, position 0."
                #endif
                let actual = Decode.fromString Decode.float "maxime"

                equal expected actual
        ]

        testList "Primitives" [

            testCase "a string works" <| fun _ ->
                let expected = Ok("maxime")
                let actual =
                    Decode.fromString Decode.string "\"maxime\""

                equal expected actual

            testCase "a float works" <| fun _ ->
                let expected = Ok(1.2)
                let actual =
                    Decode.fromString Decode.float "1.2"

                equal expected actual

            testCase "a float from int works" <| fun _ ->
                let expected = Ok(1.0)
                let actual =
                    Decode.fromString Decode.float "1"

                equal expected actual

            testCase "a bool works" <| fun _ ->
                let expected = Ok(true)
                let actual =
                    Decode.fromString Decode.bool "true"

                equal expected actual

            testCase "an invalid bool output an error" <| fun _ ->
                let expected = Error("Error at: `$`\nExpecting a boolean but instead got: 2")
                let actual =
                    Decode.fromString Decode.bool "2"

                equal expected actual

            testCase "an int works" <| fun _ ->
                let expected = Ok(25)
                let actual =
                    Decode.fromString Decode.int "25"

                equal expected actual

            testCase "an invalid int [invalid range: too big] output an error" <| fun _ ->
                let expected = Error("Error at: `$`\nExpecting an int but instead got: 2147483648\nReason: Value was either too large or too small for an int")
                let actual =
                    Decode.fromString Decode.int "2147483648"

                equal expected actual

            testCase "an invalid int [invalid range: too small] output an error" <| fun _ ->
                let expected = Error("Error at: `$`\nExpecting an int but instead got: -2147483649\nReason: Value was either too large or too small for an int")
                let actual =
                    Decode.fromString Decode.int "-2147483649"

                equal expected actual

            testCase "an int64 works from number" <| fun _ ->
                let expected = Ok 1000L
                let actual =
                    Decode.fromString Decode.int64 "1000"

                equal expected actual

            testCase "an int64 works from string" <| fun _ ->
                let expected = Ok 99L
                let actual =
                    Decode.fromString Decode.int64 "\"99\""

                equal expected actual

            testCase "an int64 works output an error if incorrect string" <| fun _ ->
                let expected =
                    Error(
                        """
Error at: `$`
Expecting an int64 but instead got: "maxime"
Reason: Input string was not in a correct format.
                        """.Trim())
                let actual =
                    Decode.fromString Decode.int64 "\"maxime\""

                equal expected actual

            testCase "an uint64 works from number" <| fun _ ->
                let expected = Ok 1000UL
                let actual =
                    Decode.fromString Decode.uint64 "1000"

                equal expected actual

            testCase "an uint64 works from string" <| fun _ ->
                let expected = Ok 1000UL
                let actual =
                    Decode.fromString Decode.uint64 "\"1000\""

                equal expected actual

            testCase "an uint64 output an error if incorrect string" <| fun _ ->
                let expected =
                    Error(
                        """
Error at: `$`
Expecting an uint64 but instead got: "maxime"
Reason: Input string was not in a correct format.
                        """.Trim())
                let actual =
                    Decode.fromString Decode.uint64 "\"maxime\""

                equal expected actual

            testCase "an bigint works from number" <| fun _ ->
                let expected = Ok 12I
                let actual =
                    Decode.fromString Decode.bigint "12"

                equal expected actual

            testCase "an bigint works from string" <| fun _ ->
                let expected = Ok 12I
                let actual =
                    Decode.fromString Decode.bigint "\"12\""

                equal expected actual

            testCase "an bigint output an error if invalid string" <| fun _ ->
                let expected =
                    Error (
                        """
Error at: `$`
Expecting a bigint but instead got: "maxime"
                        """.Trim())
                let actual =
                    Decode.fromString Decode.bigint "\"maxime\""

                equal expected actual

            testCase "a datetime works" <| fun _ ->
                let expected = Ok (new DateTime(2018, 10, 1, 11, 12, 55, DateTimeKind.Utc))
                let actual =
                    Decode.fromString Decode.datetime "\"2018-10-01T11:12:55.00Z\""

                equal expected actual

            testCase "a datetime output an error if invalid string" <| fun _ ->
                let expected =
                    Error(
                        """
Error at: `$`
Expecting a date in ISO 8601 format but instead got: "invalid_string"
                        """.Trim())

                let actual =
                    Decode.fromString Decode.datetime "\"invalid_string\""

                equal expected actual

            testCase "a datetime works with TimeZone" <| fun _ ->
                let localDate = DateTime(2018, 10, 1, 11, 12, 55, DateTimeKind.Local)

                let expected = Ok (localDate)
                let json = sprintf "\"%s\"" (localDate.ToString("O"))
                let actual =
                    Decode.fromString Decode.datetime json

                equal expected actual

            testCase "a datetimeOffset works" <| fun _ ->
                let expected =
                    DateTimeOffset(2018, 7, 2, 12, 23, 45, 0, TimeSpan.FromHours(2.))
                    |> Ok
                let json = "\"2018-07-02T12:23:45+02:00\""
                let actual =
                    Decode.fromString Decode.datetimeOffset json
                equal expected actual

            testCase "a datetimeOffset returns Error if invalid format" <| fun _ ->
                let expected =
                    Error(
                        """
Error at: `$`
Expecting a date in ISO 8601 format but instead got: "2018-1-1"
                        """.Trim())
                let json = "\"2018-1-1\""
                let actual =
                    Decode.fromString Decode.datetimeOffset json

                equal expected actual
        ]

        testList "Object primitives" [

            testCase "field works" <| fun _ ->
                let json = """{ "name": "maxime", "age": 25 }"""
                let expected = Ok("maxime")

                let actual =
                    Decode.fromString (Decode.field "name" Decode.string) json

                equal expected actual

            testCase "field output an error when field is missing" <| fun _ ->
                let json = """{ "name": "maxime", "age": 25 }"""
                let expected =
                    Error(
                        """
Error at: `$.height`
Expecting an object with a field named `height` but instead got:
{
    "name": "maxime",
    "age": 25
}
                        """.Trim())

                let actual =
                    Decode.fromString (Decode.field "height" Decode.float) json

                equal expected actual

            testCase "at works" <| fun _ ->

                let json = """{ "user": { "name": "maxime", "age": 25 } }"""
                let expected = Ok "maxime"

                let actual =
                    Decode.fromString (Decode.at ["user"; "name"] Decode.string) json

                equal expected actual

            testCase "at output an error if the path failed" <| fun _ ->
                let json = """{ "user": { "name": "maxime", "age": 25 } }"""
                let expected =
                    Error(
                        """
Error at: `$.user.firstname`
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
                    Decode.fromString (Decode.at ["user"; "firstname"] Decode.string) json

                equal expected actual

            testCase "index works" <| fun _ ->
                let json = """["maxime", "alfonso", "steffen"]"""
                let expected = Ok("alfonso")

                let actual =
                    Decode.fromString (Decode.index 1 Decode.string) json

                equal expected actual

            testCase "index output an error if array is to small" <| fun _ ->
                let json = """["maxime", "alfonso", "steffen"]"""
                let expected =
                    Error(
                        """
Error at: `$.[5]`
Expecting a longer array. Need index `5` but there are only `3` entries.
[
    "maxime",
    "alfonso",
    "steffen"
]
                        """.Trim())

                let actual =
                    Decode.fromString (Decode.index 5 Decode.string) json

                equal expected actual

            testCase "index output an error if value isn't an array" <| fun _ ->
                let json = "1"
                let expected =
                    Error(
                        """
Error at: `$.[5]`
Expecting an array but instead got: 1
                        """.Trim())

                let actual =
                    Decode.fromString (Decode.index 5 Decode.string) json

                equal expected actual

        ]


        testList "Data structure" [

            testCase "list works" <| fun _ ->
                let expected = Ok([1; 2; 3])

                let actual =
                    Decode.fromString (Decode.list Decode.int) "[1, 2, 3]"

                equal expected actual

            testCase "nested lists work" <| fun _ ->
                [ [ "maxime2" ] ]
                |> List.map (fun d ->
                    d
                    |> List.map Encode.string
                    |> Encode.list)
                |> Encode.list
                |> Encode.toString 4
                |> Decode.fromString (Decode.list (Decode.list Decode.string))
                |> function Ok v -> equal [["maxime2"]] v | Error er -> failwith er

            testCase "an invalid list output an error" <| fun _ ->
                let expected = Error("Error at: `$`\nExpecting a list but instead got: 1")

                let actual =
                    Decode.fromString (Decode.list Decode.int) "1"

                equal expected actual

            testCase "array works" <| fun _ ->
                // Need to pass by a list otherwise Fable use:
                // new Int32Array([1, 2, 3]) and the test fails
                // And this would give:
                // Expected: Result { tag: 0, data: Int32Array [ 1, 2, 3 ] }
                // Actual: Result { tag: 0, data: [ 1, 2, 3 ] }
                let expected = Ok([1; 2; 3] |> List.toArray)

                let actual =
                    Decode.fromString (Decode.array Decode.int) "[1, 2, 3]"

                equal expected actual

            testCase "an invalid array output an error" <| fun _ ->
                let expected = Error("Error at: `$`\nExpecting an array but instead got: 1")

                let actual =
                    Decode.fromString (Decode.array Decode.int) "1"

                equal expected actual

            testCase "keyValuePairs works" <| fun _ ->
                let expected = Ok([("a", 1) ; ("b", 2) ; ("c", 3)])

                let actual =
                    Decode.fromString (Decode.keyValuePairs Decode.int) """{ "a": 1, "b": 2, "c": 3 }"""

                equal expected actual

            testCase "dict works" <| fun _ ->
                let expected = Ok(Map.ofList([("a", 1) ; ("b", 2) ; ("c", 3)]))

                let actual =
                    Decode.fromString (Decode.dict Decode.int) """{ "a": 1, "b": 2, "c": 3 }"""

                equal expected actual

            testCase "dict with custom decoder works" <| fun _ ->
                let expected = Ok(Map.ofList([("a", Record2.Create 1. 1.) ; ("b", Record2.Create 2. 2.) ; ("c", Record2.Create 3. 3.)]))

                let decodePoint =
                    Decode.map2 Record2.Create
                        (Decode.field "a" Decode.float)
                        (Decode.field "b" Decode.float)

                let actual =
                    Decode.fromString (Decode.dict decodePoint)
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
                let expected = Error("Error at: `$`\nExpecting an object but instead got: 1")

                let actual =
                    Decode.fromString (Decode.dict Decode.int) "1"

                equal expected actual

        ]

        testList "Inconsistent structure" [

            testCase "oneOf works" <| fun _ ->
                let expected = Ok([1; 2; 0; 4])

                let badInt =
                    Decode.oneOf [ Decode.int; Decode.nil 0 ]

                let actual =
                    Decode.fromString (Decode.list badInt) "[1,2,null,4]"

                equal expected actual


            testCase "oneOf output errors if all case fails" <| fun _ ->
                let expected =
                    Error (
                        """
I run into the following problems:

Error at: `$`
Expecting a string but instead got: 1
Error at: `$.test`
Expecting an object but instead got:
1
                        """.Trim())

                let badInt =
                    Decode.oneOf [ Decode.string; Decode.field "test" Decode.string ]

                let actual =
                    Decode.fromString (Decode.list badInt) "[1,2,null,4]"

                equal expected actual

            testCase "optional works" <| fun _ ->
                let json = """{ "name": "maxime", "age": 25 }"""

                let expectedValid = Ok(Some "maxime")
                let actualValid =
                    Decode.fromString (Decode.option (Decode.field "name" Decode.string) ) json

                equal expectedValid actualValid

                match Decode.fromString (Decode.option (Decode.field "name" Decode.int) ) json with
                | Error _ -> ()
                | Ok _ -> failwith "Expected type error for `name` field"

                let expectedMissingField = Ok(None)
                let actualMissingField =
                    Decode.fromString (Decode.option (Decode.field "height" Decode.int) ) json

                equal expectedMissingField actualMissingField

        ]

        testList "Fancy decoding" [

            testCase "null works (test on an int)" <| fun _ ->
                let expected = Ok(20)
                let actual =
                    Decode.fromString (Decode.nil 20) "null"

                equal expected actual

            testCase "null works (test on a boolean)" <| fun _ ->
                let expected = Ok(false)
                let actual =
                    Decode.fromString (Decode.nil false) "null"

                equal expected actual

            testCase "succeed works" <| fun _ ->
                let expected = Ok(7)
                let actual =
                    Decode.fromString (Decode.succeed 7) "true"

                equal expected actual

            testCase "succeed output an error if the JSON is invalid" <| fun _ ->
                #if FABLE_COMPILER
                let expected = Error("Given an invalid JSON: Unexpected token m in JSON at position 0")
                #else
                let expected = Error("Given an invalid JSON: Unexpected character encountered while parsing value: m. Path '', line 0, position 0.")
                #endif
                let actual =
                    Decode.fromString (Decode.succeed 7) "maxime"

                equal expected actual

            testCase "fail works" <| fun _ ->
                let msg = "Failing because it's fun"
                let expected = Error("Error at: `$`\nI run into a `fail` decoder: " + msg)
                let actual =
                    Decode.fromString (Decode.fail msg) "true"

                equal expected actual

            testCase "andThen works" <| fun _ ->
                let expected = Ok 1
                let infoHelp version =
                    match version with
                    | 4 ->
                        Decode.succeed 1
                    | 3 ->
                        Decode.succeed 1
                    | _ ->
                        Decode.fail <| "Trying to decode info, but version " + (version.ToString()) + "is not supported"

                let info : Decode.Decoder<int> =
                    Decode.field "version" Decode.int
                    |> Decode.andThen infoHelp

                let actual =
                    Decode.fromString info """{ "version": 3, "data": 2 }"""

                equal expected actual


            testCase "andThen generate an error if an error occuered" <| fun _ ->
                let expected =
                    Error(
                        """
Error at: `$.version`
Expecting an object with a field named `version` but instead got:
{
    "info": 3,
    "data": 2
}
                        """.Trim())
                let infoHelp version : Decode.Decoder<int> =
                    match version with
                    | 4 ->
                        Decode.succeed 1
                    | 3 ->
                        Decode.succeed 1
                    | _ ->
                        Decode.fail <| "Trying to decode info, but version " + (version.ToString()) + "is not supported"

                let info =
                    Decode.field "version" Decode.int
                    |> Decode.andThen infoHelp

                let actual =
                    Decode.fromString info """{ "info": 3, "data": 2 }"""

                equal expected actual

        ]

        testList "Mapping" [

            testCase "map works" <| fun _ ->
                let expected = Ok(6)
                let stringLength =
                    Decode.map String.length Decode.string

                let actual =
                    Decode.fromString stringLength "\"maxime\""
                equal expected actual


            testCase "map2 works" <| fun _ ->
                let expected = Ok({a = 1.; b = 2.} : Record2)

                let decodePoint =
                    Decode.map2 Record2.Create
                        (Decode.field "a" Decode.float)
                        (Decode.field "b" Decode.float)

                let actual =
                    Decode.fromString decodePoint jsonRecord

                equal expected actual

            testCase "map3 works" <| fun _ ->
                let expected = Ok({ a = 1.
                                    b = 2.
                                    c = 3. } : Record3)

                let decodePoint =
                    Decode.map3 Record3.Create
                        (Decode.field "a" Decode.float)
                        (Decode.field "b" Decode.float)
                        (Decode.field "c" Decode.float)

                let actual =
                    Decode.fromString decodePoint jsonRecord

                equal expected actual

            testCase "map4 works" <| fun _ ->
                let expected = Ok({ a = 1.
                                    b = 2.
                                    c = 3.
                                    d = 4. } : Record4)

                let decodePoint =
                    Decode.map4 Record4.Create
                        (Decode.field "a" Decode.float)
                        (Decode.field "b" Decode.float)
                        (Decode.field "c" Decode.float)
                        (Decode.field "d" Decode.float)

                let actual =
                    Decode.fromString decodePoint jsonRecord

                equal expected actual

            testCase "map5 works" <| fun _ ->
                let expected = Ok({ a = 1.
                                    b = 2.
                                    c = 3.
                                    d = 4.
                                    e = 5. } : Record5)

                let decodePoint =
                    Decode.map5 Record5.Create
                        (Decode.field "a" Decode.float)
                        (Decode.field "b" Decode.float)
                        (Decode.field "c" Decode.float)
                        (Decode.field "d" Decode.float)
                        (Decode.field "e" Decode.float)

                let actual =
                    Decode.fromString decodePoint jsonRecord

                equal expected actual

            testCase "map6 works" <| fun _ ->
                let expected = Ok({ a = 1.
                                    b = 2.
                                    c = 3.
                                    d = 4.
                                    e = 5.
                                    f = 6. } : Record6)

                let decodePoint =
                    Decode.map6 Record6.Create
                        (Decode.field "a" Decode.float)
                        (Decode.field "b" Decode.float)
                        (Decode.field "c" Decode.float)
                        (Decode.field "d" Decode.float)
                        (Decode.field "e" Decode.float)
                        (Decode.field "f" Decode.float)

                let actual =
                    Decode.fromString decodePoint jsonRecord

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
                    Decode.map7 Record7.Create
                        (Decode.field "a" Decode.float)
                        (Decode.field "b" Decode.float)
                        (Decode.field "c" Decode.float)
                        (Decode.field "d" Decode.float)
                        (Decode.field "e" Decode.float)
                        (Decode.field "f" Decode.float)
                        (Decode.field "g" Decode.float)

                let actual =
                    Decode.fromString decodePoint jsonRecord

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
                    Decode.map8 Record8.Create
                        (Decode.field "a" Decode.float)
                        (Decode.field "b" Decode.float)
                        (Decode.field "c" Decode.float)
                        (Decode.field "d" Decode.float)
                        (Decode.field "e" Decode.float)
                        (Decode.field "f" Decode.float)
                        (Decode.field "g" Decode.float)
                        (Decode.field "h" Decode.float)

                let actual =
                    Decode.fromString decodePoint jsonRecord

                equal expected actual

            testCase "map2 generate an error if invalid" <| fun _ ->
                let expected = Error("Error at: `$.a`\nExpecting a float but instead got: \"invalid_a_field\"")

                let decodePoint =
                    Decode.map2 Record2.Create
                        (Decode.field "a" Decode.float)
                        (Decode.field "b" Decode.float)

                let actual =
                    Decode.fromString decodePoint jsonRecordInvalid

                equal expected actual

        ]

        testList "object builder" [

            testCase "get.Required.Field works" <| fun _ ->
                let json = """{ "name": "maxime", "age": 25 }"""
                let expected = Ok({ fieldA = "maxime" })

                let decoder =
                    Decode.object
                        (fun get ->
                            { fieldA = get.Required.Field "name" Decode.string }
                        )

                let actual =
                    Decode.fromString decoder json

                equal expected actual

            testCase "get.Required.Field returns Error if field is missing" <| fun _ ->
                let json = """{ "age": 25 }"""
                let expected =
                    Error(
                        """
Error at: `$.name`
Expecting an object with a field named `name` but instead got:
{
    "age": 25
}
                        """.Trim())

                let decoder =
                    Decode.object
                        (fun get ->
                            { fieldA = get.Required.Field "name" Decode.string }
                        )

                let actual =
                    Decode.fromString decoder json

                equal expected actual

            testCase "get.Required.Field returns Error if type is incorrect" <| fun _ ->
                let json = """{ "name": 12, "age": 25 }"""
                let expected =
                    Error(
                        """
Error at: `$.name`
Expecting a string but instead got: 12
                        """.Trim())

                let decoder =
                    Decode.object
                        (fun get ->
                            { fieldA = get.Required.Field "name" Decode.string }
                        )

                let actual =
                    Decode.fromString decoder json

                equal expected actual

            testCase "get.Optional.Field works" <| fun _ ->
                let json = """{ "name": "maxime", "age": 25 }"""
                let expected = Ok({ optionalField = Some "maxime" })

                let decoder =
                    Decode.object
                        (fun get ->
                            { optionalField = get.Optional.Field "name" Decode.string }
                        )

                let actual =
                    Decode.fromString decoder json

                equal expected actual

            testCase "get.Optional.Field returns None value if field is missing" <| fun _ ->
                let json = """{ "age": 25 }"""
                let expected = Ok({ optionalField = None })

                let decoder =
                    Decode.object
                        (fun get ->
                            { optionalField = get.Optional.Field "name" Decode.string }
                        )

                let actual =
                    Decode.fromString decoder json

                equal expected actual

            testCase "get.Optional.Field returns None if field is null" <| fun _ ->
                let json = """{ "name": null, "age": 25 }"""
                let expected = Ok({ optionalField = None })

                let decoder =
                    Decode.object
                        (fun get ->
                            { optionalField = get.Optional.Field "name" Decode.string }
                        )

                let actual =
                    Decode.fromString decoder json

                equal expected actual

            testCase "get.Optional.Field returns Error if type is incorrect" <| fun _ ->
                let json = """{ "name": 12, "age": 25 }"""
                let expected =
                    Error(
                        """
Error at: `$.name`
Expecting a string but instead got: 12
                        """.Trim())

                let decoder =
                    Decode.object
                        (fun get ->
                            { optionalField = get.Optional.Field "name" Decode.string }
                        )

                let actual =
                    Decode.fromString decoder json

                equal expected actual


            testCase "get.Required.At works" <| fun _ ->

                let json = """{ "user": { "name": "maxime", "age": 25 } }"""
                let expected = Ok({ fieldA = "maxime" })

                let decoder =
                    Decode.object
                        (fun get ->
                            { fieldA = get.Required.At [ "user"; "name" ] Decode.string }
                        )

                let actual =
                    Decode.fromString decoder json

                equal expected actual

            testCase "get.Required.At returns Error if non-object in path" <| fun _ ->
                let json = """{ "user": "maxime" }"""
                let expected =
                    Error(
                        """
Error at: `$.user`
Expecting an object at `user` but instead got:
"maxime"
                        """.Trim())

                let decoder =
                    Decode.object
                        (fun get ->
                            { fieldA = get.Required.At [ "user"; "name" ] Decode.string }
                        )

                let actual =
                    Decode.fromString decoder json

                equal expected actual

            testCase "get.Required.At returns Error if field missing" <| fun _ ->
                let json = """{ "user": { "name": "maxime", "age": 25 } }"""
                let expected =
                    Error(
                        """
Error at: `$.user.firstname`
Expecting an object with path `user.firstname` but instead got:
{
    "user": {
        "name": "maxime",
        "age": 25
    }
}
Node `firstname` is unkown.
                        """.Trim())

                let decoder =
                    Decode.object
                        (fun get ->
                            { fieldA = get.Required.At [ "user"; "firstname" ] Decode.string }
                        )

                let actual =
                    Decode.fromString decoder json

                equal expected actual

            testCase "get.Required.At returns Error if type is incorrect" <| fun _ ->
                let json = """{ "user": { "name": 12, "age": 25 } }"""
                let expected =
                    Error(
                        """
Error at: `$.user.name`
Expecting a string but instead got: 12
                        """.Trim())

                let decoder =
                    Decode.object
                        (fun get ->
                            { fieldA = get.Required.At [ "user"; "name" ] Decode.string }
                        )

                let actual =
                    Decode.fromString decoder json

                equal expected actual

            testCase "get.Optional.At works" <| fun _ ->

                let json = """{ "user": { "name": "maxime", "age": 25 } }"""
                let expected = Ok({ optionalField = Some "maxime" })

                let decoder =
                    Decode.object
                        (fun get ->
                            { optionalField = get.Optional.At [ "user"; "name" ] Decode.string }
                        )

                let actual =
                    Decode.fromString decoder json

                equal expected actual

            testCase "get.Optional.At returns None if non-object in path" <| fun _ ->
                let json = """{ "user": "maxime" }"""
                let expected = Ok({ optionalField = None })

                let decoder =
                    Decode.object
                        (fun get ->
                            { optionalField = get.Optional.At [ "user"; "name" ] Decode.string }
                        )

                let actual =
                    Decode.fromString decoder json

                equal expected actual

            testCase "get.Optional.At returns None if field missing" <| fun _ ->
                let json = """{ "user": { "name": "maxime", "age": 25 } }"""
                let expected = Ok({ optionalField = None })

                let decoder =
                    Decode.object
                        (fun get ->
                            { optionalField = get.Optional.At [ "user"; "firstname" ] Decode.string }
                        )

                let actual =
                    Decode.fromString decoder json

                equal expected actual

            testCase "get.Optional.At returns Error if type is incorrect" <| fun _ ->
                let json = """{ "user": { "name": 12, "age": 25 } }"""
                let expected =
                    Error(
                        """
Error at: `$.user.name`
Expecting a string but instead got: 12
                        """.Trim())

                let decoder =
                    Decode.object
                        (fun get ->
                            { optionalField = get.Optional.At [ "user"; "name" ] Decode.string }
                        )

                let actual =
                    Decode.fromString decoder json

                equal expected actual

            testCase "complex object builder works" <| fun _ ->
                let expected =
                    Ok(User.Create 67 "" "user@mail.com" 0)

                let userDecoder =
                    Decode.object
                        (fun get ->
                            { Id = get.Required.Field "id" Decode.int
                              Name = get.Optional.Field "name" Decode.string
                                        |> Option.defaultValue ""
                              Email = get.Required.Field "email" Decode.string
                              Followers = 0 }
                        )

                let actual =
                    Decode.fromString
                        userDecoder
                        """{ "id": 67, "email": "user@mail.com" }"""

                equal expected actual

        ]

        testList "Auto" [
            testCase "Auto.Decode.fromString works" <| fun _ ->
                let json =
                    { a = 5
                      b = "bar"
                      c = [false, 3; true, 5; false, 10]
                      d = [|Some(Foo 14); None|]
                      e = Map [("oh", { a = 2.; b = 2. }); ("ah", { a = -1.5; b = 0. })]
                      f = System.DateTime.Now
                    } |> Encode.Auto.toString 4
                // printfn "AUTO ENCODED %s" json
                let r2 = Decode.Auto.fromString<Record9>(json)
                equal 5 r2.a
                equal "bar" r2.b
                equal [false, 3; true, 5; false, 10] r2.c
                equal (Some(Foo 14)) r2.d.[0]
                equal None r2.d.[1]
                equal -1.5 (Map.find "ah" r2.e).a
                equal 2.   (Map.find "oh" r2.e).b

            testCase "Auto serialization works with recursive types" <| fun _ ->
                let len xs =
                    let rec lenInner acc = function
                        | Cons(_,rest) -> lenInner (acc + 1) rest
                        | Nil -> acc
                    lenInner 0 xs
                let li = Cons(1, Cons(2, Cons(3, Nil)))
                let json = Encode.Auto.toString 4 li
                // printfn "AUTO ENCODED MYLIST %s" json
                let li2 = Decode.Auto.fromString(json, typeof< MyList<int> >) :?> MyList<int>
                len li2 |> equal 3
                match li with
                | Cons(i1, Cons(i2, Cons(i3, Nil))) -> i1 + i2 + i3
                | Cons(i,_) -> i
                | Nil -> 0
                |> equal 6

            testCase "Auto decoders works for string" <| fun _ ->
                let value = "maxime"
                let json = Encode.Auto.toString 4 value
                let res = Decode.Auto.fromString<string>(json)
                equal value res

            testCase "Auto decoders works for guid" <| fun _ ->
                let value = Guid.NewGuid()
                let json = Encode.Auto.toString 4 value
                let res = Decode.Auto.fromString<Guid>(json)
                equal value res

            testCase "Auto decoders works for int" <| fun _ ->
                let value = 12
                let json = Encode.Auto.toString 4 value
                let res = Decode.Auto.fromString<int>(json)
                equal value res

            // TODO: Use an int64 value that exceeds int32 capacity (also for uint64)
            testCase "Auto decoders works for int64" <| fun _ ->
                let value = 12L
                let json = Encode.Auto.toString 4 value
                let res = Decode.Auto.fromString<int64>(json)
                equal value res

            testCase "Auto decoders works for uint64" <| fun _ ->
                let value = 12UL
                let json = Encode.Auto.toString 4 value
                let res = Decode.Auto.fromString<uint64>(json)
                equal value res

            testCase "Auto decoders works for bigint" <| fun _ ->
                let value = 12I
                let json = Encode.Auto.toString 4 value
                let res = Decode.Auto.fromString<bigint>(json)
                equal value res

            testCase "Auto decoders works for bool" <| fun _ ->
                let value = false
                let json = Encode.Auto.toString 4 value
                let res = Decode.Auto.fromString<bool>(json)
                equal value res

            testCase "Auto decoders works for float" <| fun _ ->
                let value = 12.
                let json = Encode.Auto.toString 4 value
                let res = Decode.Auto.fromString<float>(json)
                equal value res

            testCase "Auto decoders works for decimal" <| fun _ ->
                let value = 0.7833M
                let json = Encode.Auto.toString 4 value
                let res = Decode.Auto.fromString<decimal>(json)
                equal value res

            testCase "Auto decoders works for datetime" <| fun _ ->
                let value = DateTime.Now
                let json = Encode.Auto.toString 4 value
                let res = Decode.Auto.fromString<DateTime>(json)
                equal value.Date res.Date
                equal value.Hour res.Hour
                equal value.Minute res.Minute
                equal value.Second res.Second

            testCase "Auto decoders works for datetime UTC" <| fun _ ->
                let value = DateTime.UtcNow
                let json = Encode.Auto.toString 4 value
                let res = Decode.Auto.fromString<DateTime>(json)
                // printfn "SOURCE %A JSON %s OUTPUT %A (kind %A)" value json res res.Kind
                let res =
                    // TODO: Fable and .NET return different kinds when decoding DateTime, review
                    match res.Kind with
                    | DateTimeKind.Utc -> res
                    | DateTimeKind.Local -> res.ToUniversalTime()
                    | _ (* Unespecified *) -> res.ToLocalTime().ToUniversalTime()
                equal value.Date res.Date
                equal value.Hour res.Hour
                equal value.Minute res.Minute
                equal value.Second res.Second

            testCase "Auto decoders works for datetimeOffset" <| fun _ ->
                let value = DateTimeOffset.Now
                let json = Encode.Auto.toString 4 value
                let res = Decode.Auto.fromString<DateTimeOffset>(json).ToLocalTime()
                equal value.Date res.Date
                equal value.Hour res.Hour
                equal value.Minute res.Minute
                equal value.Second res.Second

            testCase "Auto decoders works for datetimeOffset UTC" <| fun _ ->
                let value = DateTimeOffset.UtcNow
                let json = Encode.Auto.toString 4 value
                let res = Decode.Auto.fromString<DateTimeOffset>(json).ToUniversalTime()
                // printfn "SOURCE %A JSON %s OUTPUT %A" value json res
                equal value.Date res.Date
                equal value.Hour res.Hour
                equal value.Minute res.Minute
                equal value.Second res.Second

            testCase "Auto decoders works for list" <| fun _ ->
                let value = [1; 2; 3; 4]
                let json = Encode.Auto.toString 4 value
                let res = Decode.Auto.fromString<int list>(json)
                equal value res

            testCase "Auto decoders works for array" <| fun _ ->
                let value = [| 1; 2; 3; 4 |]
                let json = Encode.Auto.toString 4 value
                let res = Decode.Auto.fromString<int array>(json)
                equal value res

            testCase "Auto decoders works for option None" <| fun _ ->
                let value = None
                let json = Encode.Auto.toString 4 value
                let res = Decode.Auto.fromString<int option>(json)
                equal value res

            testCase "Auto decoders works for option Some" <| fun _ ->
                let value = Some 5
                let json = Encode.Auto.toString 4 value
                let res = Decode.Auto.fromString<int option>(json)
                equal value res

            testCase "Auto decoders works for null" <| fun _ ->
                let value = null
                let json = Encode.Auto.toString 4 value
                let res = Decode.Auto.fromString<obj>(json)
                equal value res

            testCase "Auto decoders works even if type is determined by the compiler" <| fun _ ->
                let value = [1; 2; 3; 4]
                let json = Encode.Auto.toString 4 value
                let res = Decode.Auto.fromString<_>(json)
                equal value res

            testCase "Auto.FromString works with camelCase" <| fun _ ->
                let json = """{ "id" : 0, "name": "maxime", "email": "mail@domain.com", "followers": 0 }"""
                let user = Decode.Auto.fromString<User>(json, isCamelCase=true)
                equal "maxime" user.Name
                equal 0 user.Id
                equal 0 user.Followers
                equal "mail@domain.com" user.Email
        ]
    ]
