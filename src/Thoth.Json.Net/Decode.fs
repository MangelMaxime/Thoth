module Thot.Json.Net.Decode

open Newtonsoft.Json
open Newtonsoft.Json.Linq
open System
open System.IO

module Helpers =

    let anyToString (token: JToken) : string =
        use stream = new StringWriter()
        use jsonWriter = new JsonTextWriter(
                                stream,
                                Formatting = Formatting.Indented,
                                Indentation = 4 )

        token.WriteTo(jsonWriter)
        stream.ToString()

type DecoderError =
    | BadPrimitive of string * JToken
    | BadPrimitiveExtra of string * JToken * string
    | BadField of string * JToken
    | BadPath of string * JToken * string
    | TooSmallArray of string * JToken
    | FailMessage of string
    | BadOneOf of string list

type Decoder<'T> = JToken -> Result<'T, DecoderError>

let private genericMsg msg value newLine =
    try
        "Expecting "
            + msg
            + " but instead got:"
            + (if newLine then "\n" else " ")
            + (Helpers.anyToString value)
    with
        | _ ->
            "Expecting "
            + msg
            + " but decoder failed. Couldn't report given value due to circular structure."
            + (if newLine then "\n" else " ")

let private errorToString =
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

let unwrap (decoder : Decoder<'T>) (value : JToken) : 'T =
    match decoder value with
    | Ok success ->
        success
    | Error error ->
        failwith (errorToString error)

///////////////
// Runners ///
/////////////

[<Obsolete("Thot.Json library is being deprecated please use Thoth.Json")>]
let decodeValue (decoder : Decoder<'T>) =
    fun value ->
        try
            match decoder value with
            | Ok success ->
                Ok success
            | Error error ->
                Error (errorToString error)
        with
            | ex ->
                Error ex.Message

[<Obsolete("Thot.Json library is being deprecated please use Thoth.Json")>]
let decodeString (decoder : Decoder<'T>) =
    fun value ->
        try
            let json = Newtonsoft.Json.Linq.JValue.Parse value
            decodeValue decoder json
        with
            | ex ->
                Error("Given an invalid JSON: " + ex.Message)

//////////////////
// Primitives ///
////////////////

let string : Decoder<string> =
    fun token ->
        if token.Type = JTokenType.String then
            Ok(token.Value<string>())
        else
            BadPrimitive("a string", token) |> Error

let int : Decoder<int> =
    fun token ->
        if token.Type <> JTokenType.Integer then
            BadPrimitive("an int", token) |> Error
        else
            try
                let value = token.Value<int>()
                Ok(value)
            with
                | _ -> BadPrimitiveExtra("an int", token, "Value was either too large or too small for an int") |> Error

let bool : Decoder<bool> =
    fun token ->
        if token.Type = JTokenType.Boolean then
            Ok(token.Value<bool>())
        else
            BadPrimitive("a boolean", token) |> Error

let float : Decoder<float> =
    fun token ->
        if token.Type = JTokenType.Float then
            Ok(token.Value<float>())
        else
            BadPrimitive("a float", token) |> Error

/////////////////////////
// Object primitives ///
///////////////////////


let field (fieldName: string) (decoder : Decoder<'value>) : Decoder<'value> =
    fun token ->
        let errorMsg = BadField ("an object with a field named `" + fieldName + "`", token) |> Error
        if token.Type = JTokenType.Object then
            let fieldValue = token.Item(fieldName)
            if isNull fieldValue then
                errorMsg
            else
                decoder fieldValue
        else
            errorMsg

let at (fieldNames: string list) (decoder : Decoder<'value>) : Decoder<'value> =
    fun token ->
        let mutable cValue = token
        try
            for fieldName in fieldNames do
                let currentNode = cValue.Item(fieldName)
                if isNull currentNode then
                    failwith fieldName
                else
                    cValue <- currentNode
            unwrap decoder cValue |> Ok
        with
            | ex ->
                let msg = "an object with path `" + (String.concat "." fieldNames) + "`"
                BadPath (msg, token, ex.Message)
                |> Error

let index (requestedIndex: int) (decoder : Decoder<'value>) : Decoder<'value> =
    fun token ->
        if token.Type = JTokenType.Array then
            let vArray = token.Value<JArray>()
            if requestedIndex < vArray.Count then
                unwrap decoder (vArray.[requestedIndex]) |> Ok
            else
                let msg =
                    "a longer array. Need index `"
                        + (requestedIndex.ToString())
                        + "` but there are only `"
                        + (vArray.Count.ToString())
                        + "` entries"

                TooSmallArray(msg, token)
                |> Error
        else
            BadPrimitive("an array", token)
            |> Error

// let nullable (d1: Decoder<'value>) : Resul<'value option, DecoderError> =

// //////////////////////
// // Data structure ///
// ////////////////////

let list (decoder : Decoder<'value>) : Decoder<'value list> =
    fun token ->
        if token.Type = JTokenType.Array then
            token.Value<JArray>().Values()
            |> Seq.map (unwrap decoder)
            |> Seq.toList
            |> Ok
        else
            BadPrimitive ("a list", token)
            |> Error

let array (decoder : Decoder<'value>) : Decoder<'value array> =
    fun token ->
        if token.Type = JTokenType.Array then
            token.Value<JArray>().Values()
            |> Seq.map (unwrap decoder)
            |> Seq.toArray
            |> Ok
        else
            BadPrimitive ("an array", token)
            |> Error

let keyValuePairs (decoder : Decoder<'value>) : Decoder<(string * 'value) list> =
    fun token ->
        if token.Type = JTokenType.Object then
            let value = token.Value<JObject>()

            value.Properties()
            |> Seq.map (fun prop ->
                (prop.Name, value.SelectToken(prop.Name) |> unwrap decoder)
            )
            |> Seq.toList
            |> Ok
        else
            BadPrimitive ("an object", token)
            |> Error


//////////////////////////////
// Inconsistent Structure ///
////////////////////////////

let option (d1 : Decoder<'value>) : Decoder<'value option> =
    fun value ->
        match decodeValue d1 value with
        | Ok v -> Ok (Some v)
        | Error _ -> Ok None

let oneOf (decoders : Decoder<'value> list) : Decoder<'value> =
    fun value ->
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

let nil (output : 'a) : Decoder<'a> =
    fun token ->
        if token.Type = JTokenType.Null then
            Ok output
        else
            BadPrimitive("null", token) |> Error

let value v = Ok v

let succeed (output : 'a) : Decoder<'a> =
    fun _ ->
        Ok output

let fail (msg: string) : Decoder<'a> =
    fun _ ->
        FailMessage msg |> Error

let andThen (cb: 'a -> Decoder<'b>) (decoder : Decoder<'a>) : Decoder<'b> =
    fun value ->
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

let dict (decoder : Decoder<'value>) : Decoder<Map<string, 'value>> =
    map Map.ofList (keyValuePairs decoder)

////////////////
// Pipeline ///
//////////////

let custom d1 d2 = map2 (|>) d1 d2

let hardcoded<'a, 'b> : 'a -> Decoder<('a -> 'b)> -> JToken -> Result<'b,DecoderError> = succeed >> custom

let required (key : string) (valDecoder : Decoder<'a>) (decoder : Decoder<'a -> 'b>) : Decoder<'b> =
    custom (field key valDecoder) decoder

let requiredAt (path : string list) (valDecoder : Decoder<'a>) (decoder : Decoder<'a -> 'b>) : Decoder<'b> =
    custom (at path valDecoder) decoder

[<Obsolete("Thot.Json library is being deprecated please use Thoth.Json")>]
let decode output value = succeed output value

/// Convert a `Decoder<Result<x, 'a>>` into a `Decoder<'a>`
let resolve d1 : Decoder<'a> =
    fun value ->
        andThen id d1 value

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
    |> andThen handleResult

let optional key valDecoder fallback decoder =
    custom (optionalDecoder (field key value) valDecoder fallback) decoder

let optionalAt path valDecoder fallback decoder =
    custom (optionalDecoder (at path value) valDecoder fallback) decoder
