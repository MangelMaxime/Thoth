namespace Thoth.Json

[<RequireQualifiedAccess>]
module Encode =

    open Fable.Import
    open Fable.Core.JsInterop

    let inline internal padLeft2 c =  (fun (x: string) -> x.PadLeft(2, c)) << string

    /// **Description**
    /// Represents a JavaScript value
    type Value = obj

    type Encoder<'T> = 'T -> Value

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
    let inline string (value : string) : Value =
        box value

    ///**Description**
    /// Encode a GUID
    ///
    ///**Parameters**
    ///  * `value` - parameter of type `System.Guid`
    ///
    ///**Output Type**
    ///  * `Value`
    ///
    ///**Exceptions**
    ///
    let guid (value : System.Guid) : Value =
        value.ToString() |> string

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
    let inline int (value : int) : Value =
        box value

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
    let inline float (value : float) : Value =
        box value

    ///**Description**
    /// Encode a Decimal. (Currently decimal gets converted to float.)
    ///
    ///**Parameters**
    ///  * `value` - parameter of type `decimal`
    ///
    ///**Output Type**
    ///  * `Value`
    ///
    ///**Exceptions**
    ///
    let decimal (value : decimal) : Value =
        // TODO: This is OK for now because Fable just use JS number for decimals
        // but in the future we should use another format to keep precision
        FSharp.Core.Operators.float value |> float

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
        box null

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
    let inline bool (value : bool) : Value =
        box value

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
    let object (values : (string * Value) seq) : Value =
        let o = obj()
        for (key, value) in values do
            o?(key) <- value
        box o

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
    let inline array (values : array<Value>) : Value =
        box values

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
    let inline list (values : Value list) : Value =
        // Don't use List.toArray as it may create a typed array
        box (JS.Array.from(box values :?> JS.Iterable<Value>))

    ///**Description**
    /// Encode a dictionary
    ///**Parameters**
    ///  * `values` - parameter of type `Map<string, Value>`
    ///
    ///**Output Type**
    ///  * `Value`
    ///
    ///**Exceptions**
    ///
    let dict (values : Map<string, Value>) : Value =
        values
        |> Map.toList
        |> object

    let bigint (value : bigint) : Value =
        box (value.ToString())

    let datetimeOffset (value : System.DateTimeOffset) : Value =
        let offset =
            let sign =
                if value.Offset.Hours < 0 then
                    "-"
                else
                    "+"
            let hours = padLeft2 '0' (System.Math.Abs(value.Offset.Hours))
            let minutes = padLeft2 '0' value.Offset.Minutes
            sign + hours + ":" + minutes

        Operators.string value.Year
            + "-"
            + padLeft2 '0' (Operators.string value.Month)
            + "-"
            + padLeft2 '0' (Operators.string value.Day)
            + "T"
            + padLeft2 '0' (Operators.string value.Hour)
            + ":"
            + padLeft2 '0' (Operators.string value.Minute)
            + ":"
            + padLeft2 '0' (Operators.string value.Second)
            + offset
        |> string

    let int64 (value : int64) : Value =
        box (value.ToString())

    let uint64 (value : uint64) : Value =
        box (value.ToString())

    let tuple2
            (enc1 : Encoder<'T1>)
            (enc2 : Encoder<'T2>)
            (v1, v2) : Value =
        box [| enc1 v1
               enc2 v2 |]

    let tuple3
            (enc1 : Encoder<'T1>)
            (enc2 : Encoder<'T2>)
            (enc3 : Encoder<'T3>)
            (v1, v2, v3) : Value =
        box [| enc1 v1
               enc2 v2
               enc3 v3 |]

    let tuple4
            (enc1 : Encoder<'T1>)
            (enc2 : Encoder<'T2>)
            (enc3 : Encoder<'T3>)
            (enc4 : Encoder<'T4>)
            (v1, v2, v3, v4) : Value =
        box [| enc1 v1
               enc2 v2
               enc3 v3
               enc4 v4 |]

    let tuple5
            (enc1 : Encoder<'T1>)
            (enc2 : Encoder<'T2>)
            (enc3 : Encoder<'T3>)
            (enc4 : Encoder<'T4>)
            (enc5 : Encoder<'T5>)
            (v1, v2, v3, v4, v5) : Value =
        box [| enc1 v1
               enc2 v2
               enc3 v3
               enc4 v4
               enc5 v5 |]

    let tuple6
            (enc1 : Encoder<'T1>)
            (enc2 : Encoder<'T2>)
            (enc3 : Encoder<'T3>)
            (enc4 : Encoder<'T4>)
            (enc5 : Encoder<'T5>)
            (enc6 : Encoder<'T6>)
            (v1, v2, v3, v4, v5, v6) : Value =
        box [| enc1 v1
               enc2 v2
               enc3 v3
               enc4 v4
               enc5 v5
               enc6 v6 |]

    let tuple7
            (enc1 : Encoder<'T1>)
            (enc2 : Encoder<'T2>)
            (enc3 : Encoder<'T3>)
            (enc4 : Encoder<'T4>)
            (enc5 : Encoder<'T5>)
            (enc6 : Encoder<'T6>)
            (enc7 : Encoder<'T7>)
            (v1, v2, v3, v4, v5, v6, v7) : Value =
        box [| enc1 v1
               enc2 v2
               enc3 v3
               enc4 v4
               enc5 v5
               enc6 v6
               enc7 v7 |]

    let tuple8
            (enc1 : Encoder<'T1>)
            (enc2 : Encoder<'T2>)
            (enc3 : Encoder<'T3>)
            (enc4 : Encoder<'T4>)
            (enc5 : Encoder<'T5>)
            (enc6 : Encoder<'T6>)
            (enc7 : Encoder<'T7>)
            (enc8 : Encoder<'T8>)
            (v1, v2, v3, v4, v5, v6, v7, v8) : Value =
        box [| enc1 v1
               enc2 v2
               enc3 v3
               enc4 v4
               enc5 v5
               enc6 v6
               enc7 v7
               enc8 v8 |]

    /// **Description**
    ///
    /// **Parameters**
    ///   * `value` - parameter of type `System.DateTime`
    ///
    /// **Output Type**
    ///   * `Value`
    ///
    /// **Exceptions**
    ///
    let datetime (value : System.DateTime) : Value =
        value.ToString("O") |> string

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
    let toString (space: int) (value: Value) : string =
        JS.JSON.stringify(value, !!null, space)

    ///**Description**
    /// Encode an option
    ///**Parameters**
    ///  * `encoder` - parameter of type `'a -> Value`
    ///
    ///**Output Type**
    ///  * `'a option -> Value`
    ///
    ///**Exceptions**
    ///
    let option (encoder : 'a -> Value) =
        Option.map encoder >> Option.defaultWith (fun _ -> nil)

    type Auto =
        static member toString(space : int, value : obj, ?forceCamelCase : bool) : string =
            JS.JSON.stringify(value, (fun _ v ->
                match v with
                // Match string before so it's not considered an IEnumerable
                | :? string -> v
                | :? System.Collections.IEnumerable ->
                    if JS.Array.isArray(v)
                    then v
                    else JS.Array.from(v :?> JS.Iterable<obj>) |> box
                | _ ->
                    if defaultArg forceCamelCase false && Decode.Helpers.isObject v then
                        let replacement = createObj []
                        for key in Decode.Helpers.objectKeys v do
                            replacement?(key.[..0].ToLowerInvariant() + key.[1..]) <- value?(key)
                        replacement
                    else
                        v
            ), space)

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
    [<System.Obsolete("Please use toString instead")>]
    let encode (space: int) (value: Value) : string = toString space value
