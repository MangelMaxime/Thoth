module Thot.Json.Decode

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

module Helpers =

    [<Emit("typeof $0 === 'string'")>]
    let isString (_ : obj) : bool = jsNative

    [<Emit("typeof $0 === 'boolean'")>]
    let isBoolean (_ : obj) : bool = jsNative

    [<Emit("typeof $0 === 'number'")>]
    let isNumber (_ : obj) : bool = jsNative

    [<Emit("$0 instanceof Array")>]
    let isArray (_ : obj) : bool = jsNative

    [<Emit("Number.isNaN($0)")>]
    let isNaN (_: obj) : bool = jsNative

    [<Emit("-2147483647 < $0 && $0 < 2147483647 && ($0 | 0) === $0")>]
    let isValidIntRange (_: obj) : bool = jsNative

    [<Emit("isFinite($0) && !($0 % 1)")>]
    let isIntFinite (_: obj) : bool = jsNative

    [<Emit("($0 !== undefined)")>]
    let isDefined (_: obj) : bool = jsNative

    [<Emit("JSON.stringify($0, null, 4) + ''")>]
    let anyToString (_: obj) : string= jsNative

type PrimitiveError =
    { Msg : string
      Value : obj }

type DecoderError =
    | BadPrimitive of string * obj
    | BadPrimitiveExtra of string * obj * string
    | BadField of string * obj
    | BadPath of string * obj * string
    | TooSmallArray of string * obj

type Decoder<'T> = obj -> Result<'T, DecoderError>

let inline genericMsg msg value newLine =
    "Expecting "
        + msg
        + " but instead got:"
        + (if newLine then "\n" else " ")
        + (Helpers.anyToString value)

let errorToString =
    function
    | BadPrimitive (msg, value) ->
        genericMsg msg value false
    | BadPrimitiveExtra (msg, value, reason) ->
        genericMsg msg value false + "\nReason: " + reason
    | BadField (msg, value) ->
        genericMsg msg value true
    | BadPath (msg, value, fieldName) ->
        genericMsg msg value true + ("\nNode `" + fieldName + "` is unkown.")
    | TooSmallArray (msg, value) ->
        "Expecting " + msg + ".\n" + (Helpers.anyToString value)

let unwrap (decoder : Decoder<'T>) (value : obj) : 'T =
    match decoder value with
    | Ok success ->
        success
    | Error error ->
        failwith (errorToString error)

///////////////
// Runners ///
/////////////

let decodeValue (decoder : Decoder<'T>) (value : obj) : Result<'T, string> =
    try
        match decoder value with
        | Ok success ->
            Ok success
        | Error error ->
            Error (errorToString error)
    with
        | ex -> Error ex.Message

let decodeString (decoder : Decoder<'T>) (value : string) : Result<'T, string> =
    try
        let json = JS.JSON.parse value
        decodeValue decoder json
    with
        | ex ->
            Error("Given an invalid JSON: " + ex.Message)

//////////////////
// Primitives ///
////////////////

let string (value: obj) : Result<string, DecoderError> =
    if Helpers.isString value then
        Ok(unbox<string> value)
    else
        BadPrimitive("a string", value) |> Error

let int (value: obj) : Result<int, DecoderError> =
    if not (Helpers.isNumber value)  then
        BadPrimitive("an int", value) |> Error
    else
        if not (Helpers.isValidIntRange value) then
            BadPrimitiveExtra("an int", value, "Invalid range") |> Error
        else
            Ok(unbox<int> value)

let nil (output : obj) (value: obj) : Result<obj, DecoderError> =
    if isNull value then
        Ok output
    else
        BadPrimitive("null", value) |> Error

let bool (value: obj) : Result<bool, DecoderError> =
    if Helpers.isBoolean value then
        Ok(unbox<bool> value)
    else
        BadPrimitive("a boolean", value) |> Error

let float (value: obj) : Result<float, DecoderError> =
    if Helpers.isNumber value then
        Ok(unbox<float> value)
    else
        BadPrimitive("a float", value) |> Error


/////////////////////////
// Object primitives ///
///////////////////////

let field (fieldName: string) (decoder : Decoder<'value>) (value: obj) : Result<'value, DecoderError> =
    let fieldValue = value?(fieldName)
    if Helpers.isDefined fieldValue then
        decoder fieldValue
    else
        BadField ("an object with a field named `" + fieldName + "`", value)
        |> Error

let at (fieldNames: string list) (decoder : Decoder<'value>) (value: obj) : Result<'value, DecoderError> =
    let mutable cValue = value
    try
        for fieldName in fieldNames do
            let currentNode = cValue?(fieldName)
            if Helpers.isDefined currentNode then
                cValue <- currentNode
            else
                failwith fieldName
        unwrap decoder cValue |> Ok
    with
        | ex ->
            let msg = "an object with path `" + (String.concat "." fieldNames) + "`"
            BadPath (msg, value, ex.Message)
            |> Error

let index (requestedIndex: int) (decoder : Decoder<'value>) (value: obj) : Result<'value, DecoderError> =
    if Helpers.isArray value then
        let vArray = unbox<obj array> value
        if requestedIndex < vArray.Length then
            unwrap decoder (vArray.[requestedIndex]) |> Ok
        else
            let msg =
                "a longer array. Need index `"
                    + (requestedIndex.ToString())
                    + "` but there are only `"
                    + (vArray.Length.ToString())
                    + "` entries"

            TooSmallArray(msg, value)
            |> Error
    else
        BadPrimitive("an array", value)
        |> Error

// let nullable (d1: Decoder<'value>) : Resul<'value option, DecoderError> =

//////////////////////
// Data structure ///
////////////////////

let list (decoder : Decoder<'value>) (value: obj) : Result<'value list, DecoderError> =
    if Helpers.isArray value then
        unbox<obj array> value
        |> Array.map (unwrap decoder)
        |> Array.toList
        |> Ok
    else
        BadPrimitive ("a list", value)
        |> Error

let array (decoder : Decoder<'value>) (value: obj) : Result<'value array, DecoderError> =
    if Helpers.isArray value then
        unbox<obj array> value
        |> Array.map (unwrap decoder)
        |> Ok
    else
        BadPrimitive ("an array", value)
        |> Error

//////////////////////////////
// Inconsistent Structure ///
////////////////////////////

let optional (d1 : Decoder<'value>) (value: obj) :Result<'value option, DecoderError> =
    match decodeValue d1 value with
    | Ok v -> Ok (Some v)
    | Error _ -> Ok None

/////////////////////
// Map functions ///
///////////////////

let map
    (ctor : 'a -> 'value)
    (d1 : Decoder<'a>) : Decoder<'value> =
    (fun value ->
        let t = unwrap d1 value
        Ok (ctor t)
    )

let map2
    (ctor : 'a -> 'b -> 'value)
    (d1 : Decoder<'a>)
    (d2 : Decoder<'b>) : Decoder<'value> =
    (fun value ->
        let t = unwrap d1 value
        let t2 = unwrap d2 value

        Ok (ctor t t2)
    )

let map3
    (ctor : 'a -> 'b -> 'c -> 'value)
    (d1 : Decoder<'a>)
    (d2 : Decoder<'b>)
    (d3 : Decoder<'c>) : Decoder<'value> =
    (fun value ->
        let v1 = unwrap d1 value
        let v2 = unwrap d2 value
        let v3 = unwrap d3 value

        Ok (ctor v1 v2 v3)
    )

let map4
    (ctor : 'a -> 'b -> 'c -> 'd -> 'value)
    (d1 : Decoder<'a>)
    (d2 : Decoder<'b>)
    (d3 : Decoder<'c>)
    (d4 : Decoder<'d>) : Decoder<'value> =
    (fun value ->
        let v1 = unwrap d1 value
        let v2 = unwrap d2 value
        let v3 = unwrap d3 value
        let v4 = unwrap d4 value

        Ok (ctor v1 v2 v3 v4)
    )

let map5
    (ctor : 'a -> 'b -> 'c -> 'd -> 'e -> 'value)
    (d1 : Decoder<'a>)
    (d2 : Decoder<'b>)
    (d3 : Decoder<'c>)
    (d4 : Decoder<'d>)
    (d5 : Decoder<'e>) : Decoder<'value> =
    (fun value ->
        let v1 = unwrap d1 value
        let v2 = unwrap d2 value
        let v3 = unwrap d3 value
        let v4 = unwrap d4 value
        let v5 = unwrap d5 value

        Ok (ctor v1 v2 v3 v4 v5)
    )

let map6
    (ctor : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'value)
    (d1 : Decoder<'a>)
    (d2 : Decoder<'b>)
    (d3 : Decoder<'c>)
    (d4 : Decoder<'d>)
    (d5 : Decoder<'e>)
    (d6 : Decoder<'f>) : Decoder<'value> =
    (fun value ->
        let v1 = unwrap d1 value
        let v2 = unwrap d2 value
        let v3 = unwrap d3 value
        let v4 = unwrap d4 value
        let v5 = unwrap d5 value
        let v6 = unwrap d6 value

        Ok (ctor v1 v2 v3 v4 v5 v6)
    )

let map7
    (ctor : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'value)
    (d1 : Decoder<'a>)
    (d2 : Decoder<'b>)
    (d3 : Decoder<'c>)
    (d4 : Decoder<'d>)
    (d5 : Decoder<'e>)
    (d6 : Decoder<'f>)
    (d7 : Decoder<'g>) : Decoder<'value> =
    (fun value ->
        let v1 = unwrap d1 value
        let v2 = unwrap d2 value
        let v3 = unwrap d3 value
        let v4 = unwrap d4 value
        let v5 = unwrap d5 value
        let v6 = unwrap d6 value
        let v7 = unwrap d7 value

        Ok (ctor v1 v2 v3 v4 v5 v6 v7)
    )

let map8
    (ctor : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'value)
    (d1 : Decoder<'a>)
    (d2 : Decoder<'b>)
    (d3 : Decoder<'c>)
    (d4 : Decoder<'d>)
    (d5 : Decoder<'e>)
    (d6 : Decoder<'f>)
    (d7 : Decoder<'g>)
    (d8 : Decoder<'h>) : Decoder<'value> =
        (fun value ->
            let v1 = unwrap d1 value
            let v2 = unwrap d2 value
            let v3 = unwrap d3 value
            let v4 = unwrap d4 value
            let v5 = unwrap d5 value
            let v6 = unwrap d6 value
            let v7 = unwrap d7 value
            let v8 = unwrap d8 value

            Ok (ctor v1 v2 v3 v4 v5 v6 v7 v8)
        )
