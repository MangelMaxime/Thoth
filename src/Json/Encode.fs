module Thot.Json.Encode

open Fable.Core.JsInterop

type Replacer = string -> obj -> obj

type Value = Value

module private FFI =

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
let encode (space: int) (value: Value) : string =
    FFI.stringify value None space
