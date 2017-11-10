module Thot.Json.Decode

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

[<Pojo>]
type DecoderError =
    { Message : string
      Parents : DecoderError list
      Value : obj }

let makeErr message value parents =
    Error({ Message = message
            Parents = parents
            Value = value })

type Decoder<'T> = obj -> Result<'T, DecoderError>

module Helpers =

    [<Emit("typeof $0 === 'string'")>]
    let isString (input : obj) : bool = jsNative

    [<Emit("typeof $0 === 'number'")>]
    let isNumber (input : obj) : bool = jsNative

    [<Emit("Number.isNaN($0)")>]
    let isNaN (x: obj) : bool = jsNative

    [<Emit("($0 !== undefined)")>]
    let isDefined (o: obj) : bool = jsNative

let string (value: obj) : Result<string, DecoderError> =
    if Helpers.isString value then
        Ok(unbox<string> value)
    else
        makeErr "a string" value []


let float (value: obj) : Result<float, DecoderError> =
    if Helpers.isNumber value then
        Ok(unbox<float> value)
    else
        printfn "error: %A" value
        makeErr "a float" value []

let field (fieldName: string) (decoder : Decoder<'value>) (value: obj) : Result<'value, DecoderError> =
    try
        let fieldValue = value?(fieldName)
        if Helpers.isDefined fieldValue then
            decoder fieldValue
        else
            makeErr ("an object with a field named `" + fieldName + "`") value []
    with
        | _ ->
            makeErr ("an object with a field named `" + fieldName + "`") value []

let decode (decoder : Decoder<'T>) (value : obj) : Result<'T, string> =
    match decoder value with
    | Ok success ->
        Ok success
    | Error error ->
        Error error.Message

let decodeString (decoder : Decoder<'T>) (value : string) : Result<'T, string> =
    try
        let json = JS.JSON.parse value
        decode decoder json
    with
        | ex ->
            Error("Given an invalid JSON: " + ex.Message)

let unwrap (decoder : Decoder<'T>) (value : obj) : 'T =
    match decoder value with
    | Ok success ->
        success
    | Error error ->
        failwith error.Message

let map2 (ctor : 'a -> 'b -> 'value) (d1 : Decoder<'a>) (d2 : Decoder<'b>) : Decoder<'value> =
    (fun value ->
        let t = unwrap d1 value
        let t2 = unwrap d2 value

        Ok (ctor t t2)
    )
