module Thot.Json.Encode

open Fable.Core.JsInterop

type Replacer = string -> obj -> obj

/// **Description**
/// Represents a JavaScript value
type Value = Value

module private FFI =

    let identity (_:'a) : Value = importMember "./Encode.js"

    let encodeNull : Value =  importMember "./Encode.js"

    let encodeObject (_ : (string * Value) list) : Value = importMember "./Encode.js"

    let stringify (_: obj) (_: Replacer option) (_: int) = importMember "./Encode.js"

    let encodeList (_ : Value list) : Value =  importMember "./Encode.js"

///**Description**
/// Encode a string
///
///**Parameters**
///  * `value` - parameter of type `string`
///
///**Output Type**
///  * `Value`
///
///**Exceptions**
///
let string (value : string) : Value =
    FFI.identity value

///**Description**
/// Encode an int
///
///**Parameters**
///  * `value` - parameter of type `int`
///
///**Output Type**
///  * `Value`
///
///**Exceptions**
///
let int (value : int) : Value =
    FFI.identity value

///**Description**
/// Encode a Float. `Infinity` and `NaN` are encoded as `null`.
///
///**Parameters**
///  * `value` - parameter of type `float`
///
///**Output Type**
///  * `Value`
///
///**Exceptions**
///
let float (value : float) : Value =
    FFI.identity value

///**Description**
/// Encode null
///
///**Parameters**
///
///**Output Type**
///  * `Value`
///
///**Exceptions**
///
let nil : Value =
    FFI.encodeNull

///**Description**
/// Encode a bool
///**Parameters**
///  * `value` - parameter of type `bool`
///
///**Output Type**
///  * `Value`
///
///**Exceptions**
///
let bool (value : bool) : Value =
    FFI.identity value

///**Description**
/// Encode an object
///
///**Parameters**
///  * `values` - parameter of type `(string * Value) list`
///
///**Output Type**
///  * `Value`
///
///**Exceptions**
///
let object (values : (string * Value) list) : Value =
    FFI.encodeObject values

///**Description**
/// Encode an array
///
///**Parameters**
///  * `values` - parameter of type `Value array`
///
///**Output Type**
///  * `Value`
///
///**Exceptions**
///
let array (values : array<Value>) : Value =
    FFI.identity values

///**Description**
/// Encode a list
///**Parameters**
///  * `values` - parameter of type `Value list`
///
///**Output Type**
///  * `Value`
///
///**Exceptions**
///
let list (values : Value list) : Value =
    FFI.encodeList values

///**Description**
/// Convert a `Value` into a prettified string.
///**Parameters**
///  * `space` - parameter of type `int` - Amount of indentation
///  * `value` - parameter of type `obj` - Value to convert
///
///**Output Type**
///  * `string`
///
///**Exceptions**
///
let encode (space: int) (value: Value) : string =
    FFI.stringify value None space
