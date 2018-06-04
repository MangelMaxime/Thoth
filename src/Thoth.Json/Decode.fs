module Thoth.Json.Decode

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

module Helpers =
    [<Emit("typeof $0")>]
    let jsTypeof (_ : obj) : string = jsNative

    let inline isString (o: obj) : bool = o :? string

    let inline isBoolean (o: obj) : bool = o :? bool

    let inline isNumber (o: obj) : bool = jsTypeof o = "number"

    let inline isArray (o: obj) : bool = JS.Array.isArray(o)

    let inline isObject (o: obj) : bool = o <> null && jsTypeof o = "object"

    let inline isNaN (o: obj) : bool = JS.Number.isNaN(!!o)

    [<Emit("-2147483648 < $0 && $0 < 2147483647 && ($0 | 0) === $0")>]
    let isValidIntRange (_: obj) : bool = jsNative

    [<Emit("isFinite($0) && !($0 % 1)")>]
    let isIntFinite (_: obj) : bool = jsNative

    [<Emit("($0 !== undefined)")>]
    let isDefined (_: obj) : bool = jsNative

    [<Emit("JSON.stringify($0, null, 4) + ''")>]
    let anyToString (_: obj) : string= jsNative

    let inline isFunction (o: obj) : bool = jsTypeof o = "function"

    let inline objectKeys (o: obj) : string seq = upcast JS.Object.keys(o)

type DecoderError =
    | BadPrimitive of string * obj
    | BadPrimitiveExtra of string * obj * string
    | BadField of string * obj
    | BadPath of string * obj * string
    | TooSmallArray of string * obj
    | FailMessage of string
    | BadOneOf of string list

type Decoder<'T> = obj -> Result<'T, DecoderError>

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

let unwrap (decoder : Decoder<'T>) (value : obj) : 'T =
    match decoder value with
    | Ok success ->
        success
    | Error error ->
        failwith (errorToString error)

///////////////
// Runners ///
/////////////

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

let decodeString (decoder : Decoder<'T>) =
    fun value ->
        try
            let json = JS.JSON.parse value
            decodeValue decoder json
        with
            | ex ->
                Error("Given an invalid JSON: " + ex.Message)

//////////////////
// Primitives ///
////////////////

let string : Decoder<string> =
    fun value ->
        if Helpers.isString value then
            Ok(unbox<string> value)
        else
            BadPrimitive("a string", value) |> Error

let int : Decoder<int> =
    fun value ->
        if not (Helpers.isNumber value)  then
            BadPrimitive("an int", value) |> Error
        else
            if not (Helpers.isValidIntRange value) then
                BadPrimitiveExtra("an int", value, "Value was either too large or too small for an int") |> Error
            else
                Ok(unbox<int> value)

let int64 : Decoder<int64> =
    fun value ->
        if Helpers.isNumber value
        then unbox<int> value |> int64 |> Ok
        elif Helpers.isString value
        then unbox<string> value |> int64 |> Ok
        else BadPrimitive("an int64", value) |> Error

let uint64 : Decoder<uint64> =
    fun value ->
        if Helpers.isNumber value
        then unbox<int> value |> uint64 |> Ok
        elif Helpers.isString value
        then unbox<string> value |> uint64 |> Ok
        else BadPrimitive("an uint64", value) |> Error

let bool : Decoder<bool> =
    fun value ->
        if Helpers.isBoolean value then
            Ok(unbox<bool> value)
        else
            BadPrimitive("a boolean", value) |> Error

let float : Decoder<float> =
    fun value ->
        if Helpers.isNumber value then
            Ok(unbox<float> value)
        else
            BadPrimitive("a float", value) |> Error

let datetime : Decoder<System.DateTime> =
    fun value ->
        if Helpers.isString value
        then System.DateTime.Parse(unbox<string> value) |> Ok
        else BadPrimitive("a date", value) |> Error

let datetimeOffset : Decoder<System.DateTimeOffset> =
    fun value ->
        if Helpers.isString value
        then System.DateTimeOffset.Parse(unbox<string> value) |> Ok
        else BadPrimitive("a date with offset", value) |> Error

/////////////////////////
// Object primitives ///
///////////////////////

let field (fieldName: string) (decoder : Decoder<'value>) : Decoder<'value> =
    fun value ->
        let fieldValue = value?(fieldName)
        if Helpers.isDefined fieldValue then
            decoder fieldValue
        else
            BadField ("an object with a field named `" + fieldName + "`", value)
            |> Error

let at (fieldNames: string list) (decoder : Decoder<'value>) : Decoder<'value> =
    fun value ->
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

let index (requestedIndex: int) (decoder : Decoder<'value>) : Decoder<'value> =
    fun value ->
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

let list (decoder : Decoder<'value>) : Decoder<'value list> =
    fun value ->
        if Helpers.isArray value then
            unbox<obj array> value
            |> Array.map (unwrap decoder)
            |> Array.toList
            |> Ok
        else
            BadPrimitive ("a list", value)
            |> Error

let array (decoder : Decoder<'value>) : Decoder<'value array> =
    fun value ->
        if Helpers.isArray value then
            unbox<obj array> value
            |> Array.map (unwrap decoder)
            |> Ok
        else
            BadPrimitive ("an array", value)
            |> Error

let keyValuePairs (decoder : Decoder<'value>) : Decoder<(string * 'value) list> =
    fun value ->
        if not (Helpers.isObject value) || Helpers.isArray value then
            BadPrimitive ("an object", value)
            |> Error
        else
            value
            |> Helpers.objectKeys
            |> Seq.map (fun key -> (key, value?(key) |> unwrap decoder))
            |> Seq.toList
            |> Ok

let tuple2 (decoder1: Decoder<'T1>) (decoder2: Decoder<'T2>) : Decoder<'T1 * 'T2> =
    fun value ->
        if Helpers.isArray value then
            let value = unbox<obj array> value
            let a = unwrap decoder1 value.[0]
            let b = unwrap decoder2 value.[1]
            Ok(a, b)
        else
            BadPrimitive ("a tuple", value)
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
    fun value ->
        if isNull value then
            Ok output
        else
            BadPrimitive("null", value) |> Error

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

let hardcoded<'a, 'b, 'c> : 'a -> Decoder<('a -> 'b)> -> 'c -> Result<'b,DecoderError> = succeed >> custom

let required (key : string) (valDecoder : Decoder<'a>) (decoder : Decoder<'a -> 'b>) : Decoder<'b> =
    custom (field key valDecoder) decoder

let requiredAt (path : string list) (valDecoder : Decoder<'a>) (decoder : Decoder<'a -> 'b>) : Decoder<'b> =
    custom (at path valDecoder) decoder

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

//////////////////
// Reflection ///
////////////////

open Microsoft.FSharp.Reflection

// As generics are erased by Fable, let's just do an unsafe cast for performance
let inline private boxDecoder (d: Decoder<'T>): Decoder<obj> =
    !!d // d >> Result.map box

let inline private unboxDecoder (d: Decoder<obj>): Decoder<'T> =
    !!d // d >> Result.map unbox

let private object (decoders: (string * Decoder<obj>)[]) (value: obj) =
    if not (Helpers.isObject value) || Helpers.isArray value then
        BadPrimitive ("an object", value) |> Error
    else
        (decoders, Ok []) ||> Array.foldBack (fun (name, decoder) acc ->
            match acc with
            | Error _ -> acc
            | Ok result ->
                field name decoder value
                |> Result.map (fun v -> v::result))

let private mixedArray msg (decoders: Decoder<obj>[]) (values: obj[]): Result<obj list, DecoderError> =
    if decoders.Length <> values.Length then
        sprintf "Expected %i %s but got %i" decoders.Length msg values.Length
        |> FailMessage |> Error
    else
        (values, decoders, Ok [])
        |||> Array.foldBack2 (fun value decoder acc ->
            match acc with
            | Error _ -> acc
            | Ok result -> decoder value |> Result.map (fun v -> v::result))

let rec private autoDecodeRecordsAndUnions (t: System.Type): Decoder<obj> =
    if FSharpType.IsRecord(t) then
        let decoders =
            FSharpType.GetRecordFields(t)
            |> Array.map (fun fi -> fi.Name, autoDecoder fi.PropertyType)
        fun value ->
            object decoders value
            |> Result.map (fun xs -> FSharpValue.MakeRecord(t, List.toArray xs))
    elif FSharpType.IsUnion(t) then
        let casesMap =
            FSharpType.GetUnionCases(t)
            |> Seq.map (fun uci ->
                uci.Name, uci.GetFields() |> Array.map (fun fi -> autoDecoder fi.PropertyType))
            |> Map
        fun (value: obj) ->
            let uci, values = FSharpValue.GetUnionFields(value, t)
            if Helpers.isString(value) then
                FSharpValue.MakeUnion(uci, [||]) |> Ok
            else
                match Map.tryFind uci.Name casesMap with
                | None -> FailMessage("Cannot find tag " + uci.Name) |> Error
                | Some decoders ->
                    mixedArray "union fields" decoders values
                    |> Result.map (fun values -> FSharpValue.MakeUnion(uci, List.toArray values))
    else
        failwith "Class types cannot be automatically deserialized"

and private autoDecoder (t: System.Type): Decoder<obj> =
    if t.IsArray then
        let decoder = t.GetElementType() |> autoDecoder
        array decoder |> boxDecoder
    elif t.IsGenericType then
        if FSharpType.IsTuple(t) then
            let decoders = FSharpType.GetTupleElements(t) |> Array.map autoDecoder
            fun value ->
                if Helpers.isArray value then
                    mixedArray "tuple elements" decoders (unbox value)
                    |> Result.map (fun xs -> FSharpValue.MakeTuple(List.toArray xs, t))
                else BadPrimitive ("an array", value) |> Error
        else
            let fullname = t.GetGenericTypeDefinition().FullName
            if fullname = typedefof<obj list>.FullName
            then t.GenericTypeArguments.[0] |> autoDecoder |> list |> boxDecoder
            elif fullname = typedefof< Map<string, obj> >.FullName
            then
                let decoder = t.GenericTypeArguments.[1] |> autoDecoder
                (array (tuple2 string decoder) >> Result.map Map) |> boxDecoder
            else autoDecodeRecordsAndUnions t
    else
        let fullname = t.FullName
        if fullname = typeof<int>.FullName
        then boxDecoder int
        elif fullname = typeof<float>.FullName
        then boxDecoder float
        elif fullname = typeof<string>.FullName
        then boxDecoder string
        elif fullname = typeof<bool>.FullName
        then boxDecoder bool
        elif fullname = typeof<int64>.FullName
        then boxDecoder int64
        elif fullname = typeof<uint64>.FullName
        then boxDecoder uint64
        elif fullname = typeof<System.DateTime>.FullName
        then boxDecoder datetime
        elif fullname = typeof<System.DateTimeOffset>.FullName
        then boxDecoder datetimeOffset
        else autoDecodeRecordsAndUnions t

type Auto =
    static member GenerateDecoder<'T>([<Inject>] ?resolver: ITypeResolver<'T>): Decoder<'T> =
        resolver.Value.ResolveType() |> autoDecoder |> unboxDecoder

    static member GenerateDecoder(t: System.Type): Decoder<obj> =
        autoDecoder t

    static member DecodeString<'T>(json: string, [<Inject>] ?resolver: ITypeResolver<'T>): 'T =
        // TODO: Cache decoder?
        let decoder = Auto.GenerateDecoder(?resolver=resolver)
        match decodeString decoder json with
        | Ok x -> x
        | Error msg -> failwith msg

    static member DecodeString(json: string, t: System.Type): obj =
        // TODO: Cache decoder?
        let decoder = Auto.GenerateDecoder(t)
        match decodeString decoder json with
        | Ok x -> x
        | Error msg -> failwith msg
