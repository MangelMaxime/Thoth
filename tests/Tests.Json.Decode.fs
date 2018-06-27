module Tests.Decode

// #if FABLE_COMPILER
open Fable.Core.JsInterop
open Thoth.Json
open Thoth.Json.Decode
// #else
// open Thoth.Json.Net
// open Thoth.Json.Net.Decode
// #endif
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
    //   e: Map<string, Record2>
      f: System.DateTimeOffset
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
                let actual = decodeValue "$" float b

                equal expected actual
            #endif

            testCase "invalid json" <| fun _ ->
                let expected : Result<float, string>= Error "Given an invalid JSON: Unexpected token m in JSON at position 0"
                let actual = decodeString float "maxime"

                equal expected actual
        ]



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
                let expected = Error("Error at: `$`\nExpecting a boolean but instead got: 2")
                let actual =
                    decodeString bool "2"

                equal expected actual

            testCase "an int works" <| fun _ ->
                let expected = Ok(25)
                let actual =
                    decodeString int "25"

                equal expected actual

            testCase "an invalid int [invalid range: too big] output an error" <| fun _ ->
                let expected = Error("Error at: `$`\nExpecting an int but instead got: 2147483648\nReason: Value was either too large or too small for an int")
                let actual =
                    decodeString int "2147483648"

                equal expected actual

            testCase "an invalid int [invalid range: too small] output an error" <| fun _ ->
                let expected = Error("Error at: `$`\nExpecting an int but instead got: -2147483649\nReason: Value was either too large or too small for an int")
                let actual =
                    decodeString int "-2147483649"

                equal expected actual

            // testCase "an int64 works from number" <| fun _ ->
            //     let expected = Ok 1000L
            //     let actual =
            //         decodeString int64 "1000"

            //     equal expected actual

//             testCase "an int64 works from string" <| fun _ ->
//                 let expected = Ok 99L
//                 let actual =
//                     decodeString int64 "\"99\""

//                 equal expected actual

//             testCase "an int64 works output an error if incorrect string" <| fun _ ->
//                 let expected =
//                     Error(
//                         """
// Error at: `$`
// Expecting an int64 but instead got: "maxime"
// Reason: Input string was not in a correct format.
//                         """.Trim())
//                 let actual =
//                     decodeString int64 "\"maxime\""

//                 equal expected actual

//             testCase "an uint64 works from number" <| fun _ ->
//                 let expected = Ok 1000UL
//                 let actual =
//                     decodeString uint64 "1000"

//                 equal expected actual

//             testCase "an uint64 works from string" <| fun _ ->
//                 let expected = Ok 1000UL
//                 let actual =
//                     decodeString uint64 "\"1000\""

//                 equal expected actual

//             testCase "an uint64 works output an error if incorrect string" <| fun _ ->
//                 let expected =
//                     Error(
//                         """
// Error at: `$`
// Expecting an uint64 but instead got: "maxime"
// Reason: Input string was not in a correct format.
//                         """.Trim())
//                 let actual =
//                     decodeString uint64 "\"maxime\""

//                 equal expected actual

            testCase "a datetime works" <| fun _ ->
                let expected = Ok (new DateTime(2018, 10, 1, 11, 12, 55, DateTimeKind.Utc))
                let actual =
                    decodeString datetime "\"2018-10-01T11:12:55.00Z\""

                equal expected actual

            testCase "a datetime output an error if invalid string" <| fun _ ->
                let expected =
                    Error(
                        """
Error at: `$`
Expecting a datetime but instead got: "invalid_string"
Reason: Input string was not in a correct format. It is recommanded to use ISO 8601 format.
                        """.Trim())

                let actual =
                    decodeString datetime "\"invalid_string\""

                equal expected actual

            testCase "a datetime works with TimeZone" <| fun _ ->
                let localDate = DateTime(2018, 10, 1, 11, 12, 55, DateTimeKind.Local)

                let expected = Ok (localDate)
                let json = sprintf "\"%s\"" (localDate.ToString("O"))
                let actual =
                    decodeString datetime json

                equal expected actual

            // testCase "a datetimeOffset works" <| fun _ ->
            //     let date1 = DateTime(2018, 6, 27, 0, 0, 0, DateTimeKind.Local)
            //     let date2 = DateTime(2018, 6, 27, 0, 0, 0, DateTimeKind.Utc)

            //     let inline padLeft2 c =  (fun (x: string) -> x.PadLeft(2, c)) << Operators.string

            //     let json =
            //         let delta = date1 - date2
            //         let hours = padLeft2 '0' (Math.Abs delta.Hours)
            //         let minutes = padLeft2 '0' delta.Minutes
            //         let sign =
            //             if delta.Hours < 0 then
            //                 "-"
            //             else
            //                 "+"
            //         let offset = sign + hours + ":" + minutes
            //         sprintf "%i/%i/%i %s"
            //             date2.Day
            //             date2.Month
            //             date2.Year
            //             offset

            //     let localDto = DateTimeOffset(date1)

            //     let expected = Ok localDto
            //     let json = sprintf "\"%s\"" (json)
            //     let actual =
            //         decodeString datetimeOffset json

            //     equal expected actual
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
Error at: `$.height`
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
Error at: `$.[5]`
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
Error at: `$.[5]`
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

            testCase "nested lists work" <| fun _ ->
                [ [ "maxime2" ] ]
                |> List.map (fun d ->
                    d
                    |> List.map Encode.string
                    |> Encode.list)
                |> Encode.list
                |> Encode.encode 4
                |> decodeString (list (list string))
                |> function Ok v -> equal [["maxime2"]] v | Error er -> failwith er

            testCase "an invalid list output an error" <| fun _ ->
                let expected = Error("Error at: `$`\nExpecting a list but instead got: 1")

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
                let expected = Error("Error at: `$`\nExpecting an array but instead got: 1")

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
                let expected = Error("Error at: `$`\nExpecting an object but instead got: 1")

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

Error at: `$`
Expecting a string but instead got: 1
Error at: `$.test`
Expecting an object but instead got:
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

                match decodeString (option (field "name" int) ) json with
                | Error _ -> ()
                | Ok _ -> failwith "Expected type error for `name` field"

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
                let expected = Error("Error at: `$`\nI run into a `fail` decoder: " + msg)
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
                        fail <| "Trying to decode info, but version " + (version.ToString()) + "is not supported"

                let info : Decoder<int> =
                    field "version" int
                    |> andThen infoHelp

                let actual =
                    decodeString info """{ "version": 3, "data": 2 }"""

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
                let infoHelp version : Decoder<int> =
                    match version with
                    | 4 ->
                        succeed 1
                    | 3 ->
                        succeed 1
                    | _ ->
                        fail <| "Trying to decode info, but version " + (version.ToString()) + "is not supported"

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
                let expected = Error("Error at: `$.a`\nExpecting a float but instead got: \"invalid_a_field\"")

                let decodePoint =
                    map2 Record2.Create
                        (field "a" float)
                        (field "b" float)

                let actual =
                    decodeString decodePoint jsonRecordInvalid

                equal expected actual

        ]

        testList "object builder" [

            testCase "get.Required.Field works" <| fun _ ->
                let json = """{ "name": "maxime", "age": 25 }"""
                let expected = Ok({ fieldA = "maxime" })

                let decoder =
                    object
                        (fun get ->
                            { fieldA = get.Required.Field "name" string }
                        )

                let actual =
                    decodeString decoder json

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
                    object
                        (fun get ->
                            { fieldA = get.Required.Field "name" string }
                        )

                let actual =
                    decodeString decoder json

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
                    object
                        (fun get ->
                            { fieldA = get.Required.Field "name" string }
                        )

                let actual =
                    decodeString decoder json

                equal expected actual

            testCase "get.Optional.Field works" <| fun _ ->
                let json = """{ "name": "maxime", "age": 25 }"""
                let expected = Ok({ optionalField = Some "maxime" })

                let decoder =
                    object
                        (fun get ->
                            { optionalField = get.Optional.Field "name" string }
                        )

                let actual =
                    decodeString decoder json

                equal expected actual

            testCase "get.Optional.Field returns None value if field is missing" <| fun _ ->
                let json = """{ "age": 25 }"""
                let expected = Ok({ optionalField = None })

                let decoder =
                    object
                        (fun get ->
                            { optionalField = get.Optional.Field "name" string }
                        )

                let actual =
                    decodeString decoder json

                equal expected actual

            testCase "get.Optional.Field returns None if field is null" <| fun _ ->
                let json = """{ "name": null, "age": 25 }"""
                let expected = Ok({ optionalField = None })

                let decoder =
                    object
                        (fun get ->
                            { optionalField = get.Optional.Field "name" string }
                        )

                let actual =
                    decodeString decoder json

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
                    object
                        (fun get ->
                            { optionalField = get.Optional.Field "name" string }
                        )

                let actual =
                    decodeString decoder json

                equal expected actual


            testCase "get.Required.At works" <| fun _ ->

                let json = """{ "user": { "name": "maxime", "age": 25 } }"""
                let expected = Ok({ fieldA = "maxime" })

                let decoder =
                    object
                        (fun get ->
                            { fieldA = get.Required.At [ "user"; "name" ] string }
                        )

                let actual =
                    decodeString decoder json

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
                    object
                        (fun get ->
                            { fieldA = get.Required.At [ "user"; "name" ] string }
                        )

                let actual =
                    decodeString decoder json

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
                    object
                        (fun get ->
                            { fieldA = get.Required.At [ "user"; "firstname" ] string }
                        )

                let actual =
                    decodeString decoder json

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
                    object
                        (fun get ->
                            { fieldA = get.Required.At [ "user"; "name" ] string }
                        )

                let actual =
                    decodeString decoder json

                equal expected actual

            testCase "get.Optional.At works" <| fun _ ->

                let json = """{ "user": { "name": "maxime", "age": 25 } }"""
                let expected = Ok({ optionalField = Some "maxime" })

                let decoder =
                    object
                        (fun get ->
                            { optionalField = get.Optional.At [ "user"; "name" ] string }
                        )

                let actual =
                    decodeString decoder json

                equal expected actual

            testCase "get.Optional.At returns None if non-object in path" <| fun _ ->
                let json = """{ "user": "maxime" }"""
                let expected = Ok({ optionalField = None })

                let decoder =
                    object
                        (fun get ->
                            { optionalField = get.Optional.At [ "user"; "name" ] string }
                        )

                let actual =
                    decodeString decoder json

                equal expected actual

            testCase "get.Optional.At returns None if field missing" <| fun _ ->
                let json = """{ "user": { "name": "maxime", "age": 25 } }"""
                let expected = Ok({ optionalField = None })

                let decoder =
                    object
                        (fun get ->
                            { optionalField = get.Optional.At [ "user"; "firstname" ] string }
                        )

                let actual =
                    decodeString decoder json

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
                    object
                        (fun get ->
                            { optionalField = get.Optional.At [ "user"; "name" ] string }
                        )

                let actual =
                    decodeString decoder json

                equal expected actual

            testCase "complex object builder works" <| fun _ ->
                let expected =
                    Ok(User.Create 67 "" "user@mail.com" 0)

                let userDecoder =
                    object
                        (fun get ->
                            { Id = get.Required.Field "id" int
                              Name = get.Optional.Field "name" string
                                        |> Option.defaultValue ""
                              Email = get.Required.Field "email" string
                              Followers = 0 }
                        )

                let actual =
                    decodeString
                        userDecoder
                        """{ "id": 67, "email": "user@mail.com" }"""

                equal expected actual

        ]

        // testList "Auto" [
        //     testCase "Auto.DecodeString works" <| fun _ ->
        //         let json =
        //             { a = 5
        //               b = "bar"
        //               c = [false, 3; true, 5; false, 10]
        //               d = [|Some(Foo 14); None|]
        //             //   e = Map [("oh", { a = 2.; b = 2. }); ("ah", { a = -1.5; b = 0. })]
        //               f = System.DateTimeOffset.Now
        //             } |> Encode.encodeAuto 4
        //         // printfn "AUTO ENCODED %s" json
        //         let r2 = Auto.DecodeString<Record9>(json)
        //         equal 5 r2.a
        //         equal "bar" r2.b
        //         equal [false, 3; true, 5; false, 10] r2.c
        //         equal (Some(Foo 14)) r2.d.[0]
        //         equal None r2.d.[1]
        //         equal -1.5 (Map.find "ah" r2.e).a
        //         equal 2.   (Map.find "oh" r2.e).b

        //     testCase "Auto serialization works with recursive types" <| fun _ ->
        //         let len xs =
        //             let rec lenInner acc = function
        //                 | Cons(_,rest) -> lenInner (acc + 1) rest
        //                 | Nil -> acc
        //             lenInner 0 xs
        //         let li = Cons(1, Cons(2, Cons(3, Nil)))
        //         let json = Encode.encodeAuto 4 li
        //         // printfn "AUTO ENCODED MYLIST %s" json
        //         let li2 = Auto.DecodeString(json, typeof< MyList<int> >) :?> MyList<int>
        //         len li2 |> equal 3
        //         match li with
        //         | Cons(i1, Cons(i2, Cons(i3, Nil))) -> i1 + i2 + i3
        //         | Cons(i,_) -> i
        //         | Nil -> 0
        //         |> equal 6

        //     testCase "Auto.DecodeString works with camelCase" <| fun _ ->
        //         let json = """{ "id" : 0, "name": "maxime", "email": "mail@domain.com", "followers": 0 }"""
        //         let user = Auto.DecodeString<User>(json, isCamelCase=true)
        //         equal "maxime" user.Name
        //         equal 0 user.Id
        //         equal 0 user.Followers
        //         equal "mail@domain.com" user.Email
        // ]
    ]
