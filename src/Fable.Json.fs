module Fable.Json

module Decode =

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

module Encode =

    open Fable.Core
    open Fable.Core.JsInterop
    open Fable.Import.JS

    type Replacer = string -> obj -> obj

    type Value = Value

    module FFI =

        let identity (_:'a) : Value = importMember "./Encode.js"

        let encodeNull : Value =  importMember "./Encode.js"

        let encodeObject (_ : (string * Value) list) : Value = importMember "./Encode.js"

        let stringify (_: obj) (_: Replacer option) (_: int) = importMember "./Encode.js"

        let encodeList (_ : Value list) : Value =  importMember "./Encode.js"

    let string (value : string) : Value =
        FFI.identity value

    let int (value : int) : Value =
        FFI.identity value

    let float (value : float) : Value =
        FFI.identity value

    let ``null`` : Value =
        FFI.encodeNull

    let bool (value : bool) : Value =
        FFI.identity value

    let object (values : (string * Value) list) : Value =
        FFI.encodeObject values

    let array (values : array<Value>) : Value =
        FFI.identity values

    let list (values : Value list) : Value =
        FFI.encodeList values

    let encode (space: int) (value: obj) : string =
        FFI.stringify value None space
