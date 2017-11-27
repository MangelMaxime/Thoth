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

    [<Emit("typeof $0 === 'function'")>]
    let isFunction (_: obj) : bool = jsNative

type PrimitiveError =
    { Msg : string
      Value : obj }

type DecoderError =
    | BadPrimitive of string * obj
    | BadPrimitiveExtra of string * obj * string
    | BadField of string * obj
    | BadPath of string * obj * string
    | TooSmallArray of string * obj
    | FailMessage of string
    | BadOneOf of string list

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
    | BadOneOf messages ->
        "I run into the following problems:\n\n" + String.concat "\n" messages
    | FailMessage msg ->
        "I run into a `fail` decoder: " + msg

let unwrap (decoder : Decoder<'T>) (value : obj) : 'T =
    match decoder value with
    | Ok success ->
        // Fable.Import.JS.console.log success
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
            Fable.Import.JS.console.log (errorToString error)
            Error (errorToString error)
    with
        | ex ->
            Fable.Import.JS.console.log (ex.Message)
            Error ex.Message

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

let option (d1 : Decoder<'value>) (value: obj) :Result<'value option, DecoderError> =
    match decodeValue d1 value with
    | Ok v -> Ok (Some v)
    | Error _ -> Ok None

let oneOf (decoders : Decoder<'value> list) (value: obj) : Result<'value, DecoderError> =
    let rec runner (decoders : Decoder<'value> list) (errors : string list) =
        match decoders with
        | head::tail ->
            match decodeValue head value with
            | Ok v ->
                Ok v
            | Error error -> runner tail (errors @ [error])
        | [] -> BadOneOf errors |> Error

    runner decoders []

//////////////////////
// Fancy decoding ///
////////////////////

let nil (output : 'a) (value: obj) : Result<'a, DecoderError> =
    if isNull value then
        Ok output
    else
        BadPrimitive("null", value) |> Error

let value v = Ok v

let succeed (output : 'a) (_: obj) : Result<'a, DecoderError> =
    Ok output

let fail (msg: string) (_:obj) : Result<'a, DecoderError> =
    FailMessage msg |> Error

let andThen (cb: 'a -> Decoder<'b>) (decoder : Decoder<'a>) (value: obj) : Result<'b, DecoderError> =
    match decodeValue decoder value with
    | Error error -> failwith error
    | Ok result ->
        cb result value

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

////////////////
// Pipeline ///
//////////////

let custom d1 d2 = map2 (|>) d1 d2

// let hardcoded = succeed >> custom

let required (key : string) (valDecoder : Decoder<'a>) (decoder : Decoder<'a -> 'b>) : Decoder<'b> =
    custom (field key valDecoder) decoder

let requiredAt (path : string list) (valDecoder : Decoder<'a>) (decoder : Decoder<'a -> 'b>) : Decoder<'b> =
    custom (at path valDecoder) decoder

let decode = succeed

let resolve<'a, 'b> : Decoder<Decoder<'a>> -> 'b -> Result<'a,DecoderError> =
    andThen id

let optionalDecoder pathDecoder valDecoder fallback =
    let nullOr decoder =
        oneOf [ decoder; nil fallback ]

    let handleResult input =
        match decodeValue pathDecoder input with
        | Ok rawValue ->
            // Field was present, so we try to decode the value
            match decodeValue (nullOr valDecoder) rawValue with
            | Ok finalResult ->
                succeed finalResult

            | Error finalErr ->
                fail finalErr

        | Error _ ->
            // Field was not present
            succeed fallback

    value
    |> handleResult

let optional key valDecoder fallback decoder =
    custom (optionalDecoder (field key value) valDecoder fallback) decoder

let optionalAt path valDecoder fallback decoder =
    custom (optionalDecoder (at path value) valDecoder fallback) decoder
