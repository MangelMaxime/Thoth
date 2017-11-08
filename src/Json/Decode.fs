module Thot.Json.Decode

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

type DecoderError =
    { Message : string
      Parents : DecoderError list
      Value : obj }

let makeErr message value parents =
    Error({ Message = message
            Parents = parents
            Value = value })

type Decoder<'T> = obj -> Result<'T, DecoderError>
type Guard<'T> = obj -> 'T

[<Emit("typeof $0 === 'string'")>]
let typeofString (input : 'a) : bool = jsNative

[<Emit("typeof $0 === 'number'")>]
let typeofNumber (input : 'a) : bool = jsNative

[<Emit("Number.isNaN($0)")>]
let isNaN (x: 'a) : bool = jsNative

let string : Decoder<string> =
    (fun value ->
        if typeofString value then
            Ok(string value)
        else
            makeErr "Must be a string" value [])

let anyNumber : Decoder<float> =
    (fun value ->
        if typeofNumber value && not (isNaN value) then
            Ok(unbox<float> value)
        else
            makeErr "Must be a number" value [])

let decode (decoder : Decoder<'T>) : Guard<'T> =
    (fun value ->
        match decoder value with
        | Ok a -> a
        | Error error ->
            failwithf "Error: %A" error
    )

type Temp<'T> = Decoder<'T> -> obj -> 'T

// let map2 (ctor : 'a -> 'b -> 'c) (d1 : Temp<'a>) (d2 : Temp<'b>) : 'c =
//     ctor d1 d2

// (fun value ->
//     decoder(value) |> unwrapResult
// )

// let map2 (f: 'a -> 'b -> 'value) (d1 : Decoder<'a>) (d2 : Decoder<'a>) : Decoder<'value> =


// let decodeString (decoder : Decoder) (json : string) :

// type Decoder2<'T> = Decoder2<'T>
