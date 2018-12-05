
namespace Thoth.Json

[<RequireQualifiedAccess>]
module Decode =

    open System.Globalization
    open Fable.Core
    open Fable.Core.JsInterop
    open Fable.Import

    type ErrorReason =
        | BadPrimitive of string * obj
        | BadType of string * obj
        | BadTypeAt of string * obj
        | BadPrimitiveExtra of string * obj * string
        | BadField of string * obj
        | BadPath of string * obj * string
        | TooSmallArray of string * obj
        | FailMessage of string
        | BadOneOf of string list

    type DecoderError = string * ErrorReason

    type Decoder<'T> = string -> obj -> Result<'T, DecoderError>

    module internal Helpers =
        [<Emit("typeof $0")>]
        let jsTypeof (_ : obj) : string = jsNative

        [<Emit("$0 instanceof SyntaxError")>]
        let isSyntaxError (_ : obj) : bool = jsNative

        let inline isString (o: obj) : bool = o :? string

        let inline isBoolean (o: obj) : bool = o :? bool

        let inline isNumber (o: obj) : bool = jsTypeof o = "number"

        let inline isArray (o: obj) : bool = JS.Array.isArray(o)

        [<Emit("Object.getPrototypeOf($0 || false) === Object.prototype")>]
        let isObject (_ : obj) : bool = jsNative

        let inline isNaN (o: obj) : bool = JS.Number.isNaN(!!o)

        let inline isNull (o: obj): bool = isNull o

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
        let inline asBool (o: obj): bool = unbox o
        let inline asInt (o: obj): int = unbox o
        let inline asFloat (o: obj): float = unbox o
        let inline asString (o: obj): string = unbox o
        let inline asArray (o: obj): obj[] = unbox o

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

    let private errorToString (path : string, error) =
        let reason =
            match error with
            | BadPrimitive (msg, value) ->
                genericMsg msg value false
            | BadType (msg, value) ->
                genericMsg msg value true
            | BadTypeAt (msg, value) ->
                genericMsg msg value true
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

        match error with
        | BadOneOf _ ->
            // Don't need to show the path here because each error case will show it's own path
            reason
        | _ ->
            "Error at: `" + path + "`\n" + reason

    exception DecoderException of DecoderError

    let unwrap (path : string) (decoder : Decoder<'T>) (value : obj) : 'T =
        match decoder path value with
        | Ok success ->
            success
        | Error error ->
            raise (DecoderException error)

    ///////////////
    // Runners ///
    /////////////

    let private decodeValueError path (decoder : Decoder<'T>) =
        fun value ->
            try
                match decoder path value with
                | Ok success ->
                    Ok success
                | Error error ->
                    Error error
            with
                | DecoderException error ->
                    Error error

    let fromValue (path : string) (decoder : Decoder<'T>) =
        fun value ->
            match decodeValueError path decoder value with
            | Ok success ->
                Ok success
            | Error error ->
                Error (errorToString error)

    let fromString (decoder : Decoder<'T>) =
        fun value ->
            try
                let json = JS.JSON.parse value
                fromValue "$" decoder json
            with
                | ex when Helpers.isSyntaxError ex ->
                    Error("Given an invalid JSON: " + ex.Message)
                | DecoderException error ->
                    errorToString error
                    |> Error

    [<System.Obsolete("Please use fromValue instead")>]
    let decodeValue (path : string) (decoder : Decoder<'T>) = fromValue path decoder

    [<System.Obsolete("Please use fromString instead")>]
    let decodeString (decoder : Decoder<'T>) = fromString decoder

    //////////////////
    // Primitives ///
    ////////////////

    let string : Decoder<string> =
        fun path value ->
            if Helpers.isString value then
                Ok(Helpers.asString value)
            else
                (path, BadPrimitive("a string", value)) |> Error

    let guid : Decoder<System.Guid> =
        fun path value ->
            if Helpers.isString value then
                match System.Guid.TryParse (Helpers.asString value) with
                | true, x -> Ok x
                | _ -> (path, BadPrimitive("a guid", value)) |> Error
            else (path, BadPrimitive("a guid", value)) |> Error

    let int : Decoder<int> =
        fun path value ->
            if Helpers.isNumber value then
                if Helpers.isValidIntRange value then
                    Ok(Helpers.asInt value)
                else
                    (path, BadPrimitiveExtra("an int", value, "Value was either too large or too small for an int")) |> Error
            else
                (path, BadPrimitive("an int", value)) |> Error


    let int64 : Decoder<int64> =
        fun path value ->
            if Helpers.isNumber value then
                Helpers.asInt value |> int64 |> Ok
            elif Helpers.isString value then
                match System.Int64.TryParse (Helpers.asString value) with
                | true, x -> Ok x
                | _ -> (path, BadPrimitive("an int64", value)) |> Error
            else (path, BadPrimitive("an int64", value)) |> Error

    let uint32 : Decoder<uint32> =
        fun path value ->
            if Helpers.isNumber value then
                let x = Helpers.asFloat value
                if x >= 0. && x <= (float System.UInt32.MaxValue) then
                    Helpers.asInt value |> uint32 |> Ok
                else
                    (path, BadPrimitiveExtra("an uint32", value, "Value was either too large or too small for an uint32")) |> Error
            elif Helpers.isString value then
                try
                    Helpers.asString value |> uint32 |> Ok
                with
                    | ex ->
                        (path, BadPrimitiveExtra("an uint32", value, ex.Message)) |> Error
            else (path, BadPrimitive("an uint32", value)) |> Error

    let uint64 : Decoder<uint64> =
        fun path value ->
            if Helpers.isNumber value then
                let x = Helpers.asFloat value
                if x >= 0. && x <= (float System.UInt64.MaxValue) then
                    Helpers.asInt value |> uint64 |> Ok
                else
                    (path, BadPrimitiveExtra("an uint64", value, "Value was either too large or too small for an uint64")) |> Error
            elif Helpers.isString value then
                try
                    Helpers.asString value |> uint64 |> Ok
                with
                    | ex ->
                        (path, BadPrimitiveExtra("an uint64", value, ex.Message)) |> Error
            else (path, BadPrimitive("an uint64", value)) |> Error

    let bigint : Decoder<bigint> =
        fun path value ->
            if Helpers.isNumber value then
                Helpers.asInt value |> bigint |> Ok
            elif Helpers.isString value then
                try
                    bigint.Parse (Helpers.asString value, CultureInfo.InvariantCulture) |> Ok
                with
                    | _ ->
                        (path, BadPrimitive("a bigint", value)) |> Error
            else
                (path, BadPrimitive("a bigint", value)) |> Error

    let bool : Decoder<bool> =
        fun path value ->
            if Helpers.isBoolean value then
                Ok(Helpers.asBool value)
            else
                (path, BadPrimitive("a boolean", value)) |> Error

    let float : Decoder<float> =
        fun path value ->
            if Helpers.isNumber value then
                Ok(Helpers.asFloat value)
            else
                (path, BadPrimitive("a float", value)) |> Error

    let decimal : Decoder<decimal> =
        fun path value ->
            if Helpers.isNumber value then
                Helpers.asFloat value |> decimal |> Ok
            elif Helpers.isString value then
                try
                    // System.Decimal.Parse (Helpers.asString value, CultureInfo.InvariantCulture)
                    // Remove CultureInfo.InvariantCulture for now, otherwise Fable output a warning
                    System.Decimal.Parse (Helpers.asString value)
                    |> Ok
                with
                    | ex ->
                        (path, BadPrimitiveExtra("a decimal", value, ex.Message)) |> Error
            else
                (path, BadPrimitive("a decimal", value)) |> Error

    let datetime : Decoder<System.DateTime> =
        fun path value ->
            if Helpers.isString value then
                match System.DateTime.TryParse (Helpers.asString value) with
                | true, x -> x.ToUniversalTime() |> Ok
                | _ -> (path, BadPrimitive("a datetime", value)) |> Error
            else
                (path, BadPrimitive("a datetime", value)) |> Error

    let datetimeOffset : Decoder<System.DateTimeOffset> =
        fun path value ->
            if Helpers.isString value then
                match System.DateTimeOffset.TryParse(Helpers.asString value) with
                | true, x -> Ok x
                | _ -> (path, BadPrimitive("a datetimeoffset", value)) |> Error
            else
                (path, BadPrimitive("a datetime", value)) |> Error

    /////////////////////////
    // Object primitives ///
    ///////////////////////

    let field (fieldName: string) (decoder : Decoder<'value>) : Decoder<'value> =
        fun path value ->
            let currentPath = path + "." + fieldName
            if Helpers.isObject value then
                let fieldValue = value?(fieldName)
                if Helpers.isDefined fieldValue then
                    decoder currentPath fieldValue
                else
                    (currentPath, BadField ("an object with a field named `" + fieldName + "`", value))
                    |> Error
            else
                (currentPath, BadType("an object", value))
                |> Error

    exception UndefinedValueException of string
    exception NonObjectTypeException

    let at (fieldNames: string list) (decoder : Decoder<'value>) : Decoder<'value> =
        fun path value ->
            let mutable cValue = value
            let mutable currentPath = path
            let mutable index = 0
            try
                for fieldName in fieldNames do
                    if Helpers.isObject cValue then
                        let currentNode = cValue?(fieldName)
                        currentPath <- currentPath + "." + fieldName
                        if Helpers.isDefined currentNode then
                            cValue <- currentNode
                        else
                            raise (UndefinedValueException fieldName)
                    else
                        raise NonObjectTypeException
                    index <- index + 1

                unwrap currentPath decoder cValue |> Ok
            with
                | NonObjectTypeException ->
                    let path = String.concat "." fieldNames.[..index-1]
                    (currentPath, BadTypeAt ("an object at `" + path + "`", cValue))
                    |> Error
                | UndefinedValueException fieldName ->
                    let msg = "an object with path `" + (String.concat "." fieldNames) + "`"
                    (currentPath, BadPath (msg, value, fieldName))
                    |> Error

    let index (requestedIndex: int) (decoder : Decoder<'value>) : Decoder<'value> =
        fun path value ->
            let currentPath = path + ".[" + (Operators.string requestedIndex) + "]"
            if Helpers.isArray value then
                let vArray = Helpers.asArray value
                if requestedIndex < vArray.Length then
                    unwrap currentPath decoder (vArray.[requestedIndex]) |> Ok
                else
                    let msg =
                        "a longer array. Need index `"
                            + (requestedIndex.ToString())
                            + "` but there are only `"
                            + (vArray.Length.ToString())
                            + "` entries"

                    (currentPath, TooSmallArray(msg, value))
                    |> Error
            else
                (currentPath, BadPrimitive("an array", value))
                |> Error


    let optional (fieldName : string) (decoder : Decoder<'value>) : Decoder<'value option> =
        fun path v ->
            match decodeValueError path (field fieldName decoder) v with
            | Ok v -> Ok (Some v)
            | Error (_, BadField _ )
            | Error (_, BadType (_, null))
            | Error (_, BadPrimitive (_, null)) -> Ok None
            | Error error ->
                raise (DecoderException error)

    let optionalAt (fieldNames : string list) (decoder : Decoder<'value>) : Decoder<'value option> =
        fun path v ->
            match decodeValueError path (at fieldNames decoder) v with
            | Ok v -> Ok (Some v)
            | Error (_, BadPath _ )
            | Error (_, BadType (_, null))
            | Error (_, BadTypeAt _)
            | Error (_, BadPrimitive (_, null)) -> Ok None
            | Error error ->
                raise (DecoderException error)

    //////////////////////
    // Data structure ///
    ////////////////////

    let list (decoder : Decoder<'value>) : Decoder<'value list> =
        fun path value ->
            if Helpers.isArray value then
                let mutable i = -1
                let tokens = Helpers.asArray value
                (Ok [], tokens) ||> Array.fold (fun acc value ->
                    i <- i + 1
                    match acc with
                    | Error _ -> acc
                    | Ok acc ->
                        match decoder (path + "[" + (i.ToString()) + "]") value with
                        | Error er -> Error er
                        | Ok value -> Ok (value::acc))
                |> Result.map List.rev
            else
                (path, BadPrimitive ("a list", value))
                |> Error

    let array (decoder : Decoder<'value>) : Decoder<'value array> =
        fun path value ->
            if Helpers.isArray value then
                let mutable i = -1
                let tokens = Helpers.asArray value
                let arr = Array.zeroCreate tokens.Length
                (Ok arr, tokens) ||> Array.fold (fun acc value ->
                    i <- i + 1
                    match acc with
                    | Error _ -> acc
                    | Ok acc ->
                        match decoder (path + "[" + (i.ToString()) + "]") value with
                        | Error er -> Error er
                        | Ok value -> acc.[i] <- value; Ok acc)
            else
                (path, BadPrimitive ("an array", value))
                |> Error

    let keyValuePairs (decoder : Decoder<'value>) : Decoder<(string * 'value) list> =
        fun path value ->
            if not (Helpers.isObject value) || Helpers.isArray value then
                (path, BadPrimitive ("an object", value))
                |> Error
            else
                value
                |> Helpers.objectKeys
                |> Seq.map (fun key -> (key, value?(key) |> unwrap path decoder))
                |> Seq.toList
                |> Ok

    //////////////////////////////
    // Inconsistent Structure ///
    ////////////////////////////

    let option (d1 : Decoder<'value>) : Decoder<'value option> =
        fun path value ->
            if Helpers.isNull value then
                Ok None
            else
                match d1 path value with
                | Ok v -> Ok (Some v)
                | Error (_, BadField _ )
                | Error (_, BadType (_, null))
                | Error (_, BadPrimitive (_, null)) -> Ok None
                | Error error -> Error error

    let oneOf (decoders : Decoder<'value> list) : Decoder<'value> =
        fun path value ->
            let rec runner (decoders : Decoder<'value> list) (errors : string list) =
                match decoders with
                | head::tail ->
                    match fromValue path head value with
                    | Ok v ->
                        Ok v
                    | Error error -> runner tail (errors @ [error])
                | [] -> (path, BadOneOf errors) |> Error

            runner decoders []

    //////////////////////
    // Fancy decoding ///
    ////////////////////

    let nil (output : 'a) : Decoder<'a> =
        fun path value ->
            if isNull value then
                Ok output
            else
                (path, BadPrimitive("null", value)) |> Error

    let value _ v = Ok v

    let succeed (output : 'a) : Decoder<'a> =
        fun _ _ ->
            Ok output

    let fail (msg: string) : Decoder<'a> =
        fun path _ ->
            (path, FailMessage msg) |> Error

    let andThen (cb: 'a -> Decoder<'b>) (decoder : Decoder<'a>) : Decoder<'b> =
        fun path value ->
            match decodeValueError path decoder value with
            | Error error ->
                raise (DecoderException error)
            | Ok result ->
                cb result path value

    /////////////////////
    // Map functions ///
    ///////////////////

    let map
        (ctor : 'a -> 'value)
        (d1 : Decoder<'a>) : Decoder<'value> =
        (fun path value ->
            let t = unwrap path d1 value
            Ok (ctor t)
        )

    let map2
        (ctor : 'a -> 'b -> 'value)
        (d1 : Decoder<'a>)
        (d2 : Decoder<'b>) : Decoder<'value> =
        (fun path value ->
            let t = unwrap path d1 value
            let t2 = unwrap path d2 value

            Ok (ctor t t2)
        )

    let map3
        (ctor : 'a -> 'b -> 'c -> 'value)
        (d1 : Decoder<'a>)
        (d2 : Decoder<'b>)
        (d3 : Decoder<'c>) : Decoder<'value> =
        (fun path value ->
            let v1 = unwrap path d1 value
            let v2 = unwrap path d2 value
            let v3 = unwrap path d3 value

            Ok (ctor v1 v2 v3)
        )

    let map4
        (ctor : 'a -> 'b -> 'c -> 'd -> 'value)
        (d1 : Decoder<'a>)
        (d2 : Decoder<'b>)
        (d3 : Decoder<'c>)
        (d4 : Decoder<'d>) : Decoder<'value> =
        (fun path value ->
            let v1 = unwrap path d1 value
            let v2 = unwrap path d2 value
            let v3 = unwrap path d3 value
            let v4 = unwrap path d4 value

            Ok (ctor v1 v2 v3 v4)
        )

    let map5
        (ctor : 'a -> 'b -> 'c -> 'd -> 'e -> 'value)
        (d1 : Decoder<'a>)
        (d2 : Decoder<'b>)
        (d3 : Decoder<'c>)
        (d4 : Decoder<'d>)
        (d5 : Decoder<'e>) : Decoder<'value> =
        (fun path value ->
            let v1 = unwrap path d1 value
            let v2 = unwrap path d2 value
            let v3 = unwrap path d3 value
            let v4 = unwrap path d4 value
            let v5 = unwrap path d5 value

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
        (fun path value ->
            let v1 = unwrap path d1 value
            let v2 = unwrap path d2 value
            let v3 = unwrap path d3 value
            let v4 = unwrap path d4 value
            let v5 = unwrap path d5 value
            let v6 = unwrap path d6 value

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
        (fun path value ->
            let v1 = unwrap path d1 value
            let v2 = unwrap path d2 value
            let v3 = unwrap path d3 value
            let v4 = unwrap path d4 value
            let v5 = unwrap path d5 value
            let v6 = unwrap path d6 value
            let v7 = unwrap path d7 value

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
            (fun path value ->
                let v1 = unwrap path d1 value
                let v2 = unwrap path d2 value
                let v3 = unwrap path d3 value
                let v4 = unwrap path d4 value
                let v5 = unwrap path d5 value
                let v6 = unwrap path d6 value
                let v7 = unwrap path d7 value
                let v8 = unwrap path d8 value

                Ok (ctor v1 v2 v3 v4 v5 v6 v7 v8)
            )

    let dict (decoder : Decoder<'value>) : Decoder<Map<string, 'value>> =
        map Map.ofList (keyValuePairs decoder)

    //////////////////////
    // Object builder ///
    ////////////////////

    type IRequiredGetter =
        abstract Field : string -> Decoder<'a> -> 'a
        abstract At : List<string> -> Decoder<'a> -> 'a
        abstract Raw : Decoder<'a> -> 'a

    type IOptionalGetter =
        abstract Field : string -> Decoder<'a> -> 'a option
        abstract At : List<string> -> Decoder<'a> -> 'a option
        abstract Raw : Decoder<'a> -> 'a option

    type IGetters =
        abstract Required: IRequiredGetter
        abstract Optional: IOptionalGetter

    let object (builder: IGetters -> 'value) : Decoder<'value> =
        fun path v ->
            builder { new IGetters with
                member __.Required =
                    { new IRequiredGetter with
                        member __.Field (fieldName : string) (decoder : Decoder<_>) =
                            match decodeValueError path (field fieldName decoder) v with
                            | Ok v -> v
                            | Error error ->
                                raise (DecoderException error)
                        member __.At (fieldNames : string list) (decoder : Decoder<_>) =
                            match decodeValueError path (at fieldNames decoder) v with
                            | Ok v -> v
                            | Error error ->
                                raise (DecoderException error)
                        member __.Raw (decoder: Decoder<_>) =
                            match decodeValueError path decoder v with
                            | Ok v -> v
                            | Error error ->
                                raise (DecoderException error) }
                member __.Optional =
                    { new IOptionalGetter with
                        member __.Field (fieldName : string) (decoder : Decoder<_>) =
                            match decodeValueError path (field fieldName decoder) v with
                            | Ok v -> Some v
                            | Error (_, BadField _ )
                            | Error (_, BadType (_, null))
                            | Error (_, BadPrimitive (_, null)) -> None
                            | Error error ->
                                raise (DecoderException error)
                        member __.At (fieldNames : string list) (decoder : Decoder<_>) =
                            if Helpers.isObject v then
                                match decodeValueError path (at fieldNames decoder) v with
                                | Ok v -> Some v
                                | Error (_, BadPath _ )
                                | Error (_, BadType (_, null))
                                | Error (_, BadTypeAt _)
                                | Error (_, BadPrimitive (_, null)) -> None
                                | Error error ->
                                    raise (DecoderException error)
                            else
                                raise (DecoderException (path, BadType ("an object", v)))
                        member __.Raw (decoder: Decoder<_>) =
                            match decodeValueError path decoder v with
                            | Ok v -> Some v
                            | Error (_, BadField _ )
                            | Error (_, BadPrimitive (_, null)) -> None
                            | Error error ->
                                raise (DecoderException error) }
            } |> Ok

    ///////////////////////
    // Tuples decoders ///
    ////////////////////

    let tuple2 (decoder1: Decoder<'T1>) (decoder2: Decoder<'T2>) : Decoder<'T1 * 'T2> =
        index 0 decoder1
        |> andThen (fun v1 ->
            index 1 decoder2
            |> andThen (fun v2 ->
                succeed (v1, v2)
            )
        )

    let tuple3 (decoder1: Decoder<'T1>)
               (decoder2: Decoder<'T2>)
               (decoder3: Decoder<'T3>) : Decoder<'T1 * 'T2 * 'T3> =
        index 0 decoder1
        |> andThen (fun v1 ->
            index 1 decoder2
            |> andThen (fun v2 ->
                index 2 decoder3
                |> andThen (fun v3 ->
                    succeed (v1, v2, v3)
                )
            )
        )

    let tuple4 (decoder1: Decoder<'T1>)
               (decoder2: Decoder<'T2>)
               (decoder3: Decoder<'T3>)
               (decoder4: Decoder<'T4>) : Decoder<'T1 * 'T2 * 'T3 * 'T4> =
        index 0 decoder1
        |> andThen (fun v1 ->
            index 1 decoder2
            |> andThen (fun v2 ->
                index 2 decoder3
                |> andThen (fun v3 ->
                    index 3 decoder4
                    |> andThen (fun v4 ->
                        succeed (v1, v2, v3, v4)
                    )
                )
            )
        )

    let tuple5 (decoder1: Decoder<'T1>)
               (decoder2: Decoder<'T2>)
               (decoder3: Decoder<'T3>)
               (decoder4: Decoder<'T4>)
               (decoder5: Decoder<'T5>) : Decoder<'T1 * 'T2 * 'T3 * 'T4 * 'T5> =
        index 0 decoder1
        |> andThen (fun v1 ->
            index 1 decoder2
            |> andThen (fun v2 ->
                index 2 decoder3
                |> andThen (fun v3 ->
                    index 3 decoder4
                    |> andThen (fun v4 ->
                        index 4 decoder5
                        |> andThen (fun v5 ->
                            succeed (v1, v2, v3, v4, v5)
                        )
                    )
                )
            )
        )

    let tuple6 (decoder1: Decoder<'T1>)
               (decoder2: Decoder<'T2>)
               (decoder3: Decoder<'T3>)
               (decoder4: Decoder<'T4>)
               (decoder5: Decoder<'T5>)
               (decoder6: Decoder<'T6>) : Decoder<'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6> =
        index 0 decoder1
        |> andThen (fun v1 ->
            index 1 decoder2
            |> andThen (fun v2 ->
                index 2 decoder3
                |> andThen (fun v3 ->
                    index 3 decoder4
                    |> andThen (fun v4 ->
                        index 4 decoder5
                        |> andThen (fun v5 ->
                            index 5 decoder6
                            |> andThen (fun v6 ->
                                succeed (v1, v2, v3, v4, v5, v6)
                            )
                        )
                    )
                )
            )
        )

    let tuple7 (decoder1: Decoder<'T1>)
               (decoder2: Decoder<'T2>)
               (decoder3: Decoder<'T3>)
               (decoder4: Decoder<'T4>)
               (decoder5: Decoder<'T5>)
               (decoder6: Decoder<'T6>)
               (decoder7: Decoder<'T7>) : Decoder<'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7> =
        index 0 decoder1
        |> andThen (fun v1 ->
            index 1 decoder2
            |> andThen (fun v2 ->
                index 2 decoder3
                |> andThen (fun v3 ->
                    index 3 decoder4
                    |> andThen (fun v4 ->
                        index 4 decoder5
                        |> andThen (fun v5 ->
                            index 5 decoder6
                            |> andThen (fun v6 ->
                                index 6 decoder7
                                |> andThen (fun v7 ->
                                    succeed (v1, v2, v3, v4, v5, v6, v7)
                                )
                            )
                        )
                    )
                )
            )
        )

    let tuple8 (decoder1: Decoder<'T1>)
               (decoder2: Decoder<'T2>)
               (decoder3: Decoder<'T3>)
               (decoder4: Decoder<'T4>)
               (decoder5: Decoder<'T5>)
               (decoder6: Decoder<'T6>)
               (decoder7: Decoder<'T7>)
               (decoder8: Decoder<'T8>) : Decoder<'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7 * 'T8> =
        index 0 decoder1
        |> andThen (fun v1 ->
            index 1 decoder2
            |> andThen (fun v2 ->
                index 2 decoder3
                |> andThen (fun v3 ->
                    index 3 decoder4
                    |> andThen (fun v4 ->
                        index 4 decoder5
                        |> andThen (fun v5 ->
                            index 5 decoder6
                            |> andThen (fun v6 ->
                                index 6 decoder7
                                |> andThen (fun v7 ->
                                    index 7 decoder8
                                    |> andThen (fun v8 ->
                                        succeed (v1, v2, v3, v4, v5, v6, v7, v8)
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )

    //////////////////
    // Reflection ///
    ////////////////

    open FSharp.Reflection

    type FieldType =
        | Optional
        | Required

        member this.ToBool
            with get () =
                match this with
                | Optional -> true
                | Required -> false

    // TODO: Same API as for Thot.Json.Net.Decoder.BoxedDecoder
    type BoxedDecoder = Decoder<obj>
    type ExtraDecoders = Map<string, BoxedDecoder>

    // As generics are erased by Fable, let's just do an unsafe cast for performance
    let inline boxDecoder (d: Decoder<'T>): BoxedDecoder =
        !!d // d >> Result.map box

    let inline unboxDecoder (d: BoxedDecoder): Decoder<'T> =
        !!d // d >> Result.map unbox

    let inline makeExtra(): ExtraDecoders = Map.empty
    let inline withInt64 (extra: ExtraDecoders): ExtraDecoders =
        Map.add typeof<int64>.FullName (boxDecoder int64) extra
    let inline withUInt64 (extra: ExtraDecoders): ExtraDecoders =
        Map.add typeof<uint64>.FullName (boxDecoder uint64) extra
    let inline withDecimal (extra: ExtraDecoders): ExtraDecoders =
        Map.add typeof<decimal>.FullName (boxDecoder decimal) extra
    let inline withBigInt (extra: ExtraDecoders): ExtraDecoders =
        Map.add typeof<bigint>.FullName (boxDecoder bigint) extra
    let inline withCustom (decoder: Decoder<'Value>) (extra: ExtraDecoders): ExtraDecoders =
        Map.add typeof<'Value>.FullName (boxDecoder decoder) extra

    // This is used to force Fable use a generic comparer for map keys
    let private toMap<'key, 'value when 'key: comparison> (xs: ('key*'value) seq) = Map.ofSeq xs
    let private toSet<'key when 'key: comparison> (xs: 'key seq) = Set.ofSeq xs

    let private autoObject (decoderInfos: (FieldType * string * BoxedDecoder)[]) (path : string) (value: obj) =
        if not (Helpers.isObject value) then
            (path, BadPrimitive ("an object", value)) |> Error
        else
            (decoderInfos, Ok []) ||> Array.foldBack (fun (fieldType, name, decoder) acc ->
                match acc with
                | Error _ -> acc
                | Ok result ->
                    match fieldType with
                    | FieldType.Optional ->
                        optional name decoder path value
                        |> Result.map (fun v -> box v::result)
                    | FieldType.Required ->
                        field name decoder path value
                        |> Result.map (fun v -> v::result))

    let private autoObject2 (keyDecoder: BoxedDecoder) (valueDecoder: BoxedDecoder) (path : string) (value: obj) =
        if not (Helpers.isObject value) then
            (path, BadPrimitive ("an object", value)) |> Error
        else
            (Ok [], Helpers.objectKeys(value)) ||> Seq.fold (fun acc name ->
                match acc with
                | Error _ -> acc
                | Ok acc ->
                    match keyDecoder path name with
                    | Error er -> Error er
                    | Ok k ->
                        match field name valueDecoder path value with
                        | Error er -> Error er
                        | Ok v -> (k,v)::acc |> Ok)

    let private mixedArray msg (decoders: BoxedDecoder[]) (path: string) (values: obj[]): Result<obj list, DecoderError> =
        if decoders.Length <> values.Length then
            (path, sprintf "Expected %i %s but got %i" decoders.Length msg values.Length
            |> FailMessage) |> Error
        else
            (values, decoders, Ok [])
            |||> Array.foldBack2 (fun value decoder acc ->
                match acc with
                | Error _ -> acc
                | Ok result -> decoder path value |> Result.map (fun v -> v::result))

    let rec private makeUnion extra isCamelCase t name (path : string) (values: obj[]) =
        match FSharpType.GetUnionCases(t) |> Array.tryFind (fun x -> x.Name = name) with
        | None -> (path, FailMessage("Cannot find case " + name + " in " + t.FullName)) |> Error
        | Some uci ->
            if values.Length = 0 then
                FSharpValue.MakeUnion(uci, [||]) |> Ok
            else
                let decoders = uci.GetFields() |> Array.map (fun fi -> autoDecoder extra isCamelCase false fi.PropertyType)
                mixedArray "union fields" decoders path values
                |> Result.map (fun values -> FSharpValue.MakeUnion(uci, List.toArray values))

    and private autoDecodeRecordsAndUnions extra (isCamelCase : bool) (isOptional : bool) (t: System.Type) : BoxedDecoder =
        if FSharpType.IsRecord(t) then
            let decoders =
                FSharpType.GetRecordFields(t)
                |> Array.map (fun fi ->
                    let name =
                        if isCamelCase then
                            fi.Name.[..0].ToLowerInvariant() + fi.Name.[1..]
                        else
                            fi.Name

                    let fieldType, propertyType =
                        if fi.PropertyType.IsGenericType then
                            let fullname = fi.PropertyType.GetGenericTypeDefinition().FullName
                            if fullname = typedefof<obj option>.FullName then
                                FieldType.Optional, fi.PropertyType.GenericTypeArguments.[0]
                            else
                                FieldType.Required, fi.PropertyType
                        else
                            FieldType.Required, fi.PropertyType

                    fieldType, name, autoDecoder extra isCamelCase fieldType.ToBool propertyType)
            fun path value ->
                autoObject decoders path value
                |> Result.map (fun xs -> FSharpValue.MakeRecord(t, List.toArray xs))

        elif FSharpType.IsUnion(t) then
            fun path (value: obj) ->
                if Helpers.isString(value) then
                    let name = Helpers.asString value
                    makeUnion extra isCamelCase t name path [||]
                elif Helpers.isArray(value) then
                    let values = Helpers.asArray value
                    let name = Helpers.asString values.[0]
                    makeUnion extra isCamelCase t name path values.[1..]
                else (path, BadPrimitive("a string or array", value)) |> Error

        else
            if isOptional then
                fun path _ ->
                    (path, BadPrimitive ("Generating an error message as the field is optional so the `option` decoders will return `None` instead of failing", null))
                    |> Error
            else
                failwithf "Cannot generate auto decoder for %s. Please pass an extra decoder." t.FullName

    and private autoDecoder (extra: ExtraDecoders) isCamelCase (isOptional : bool) (t: System.Type) : BoxedDecoder =
      let fullname = t.FullName
      match Map.tryFind fullname extra with
      | Some decoder -> decoder
      | None ->
        if t.IsArray then
            let decoder = t.GetElementType() |> autoDecoder extra isCamelCase false
            array decoder |> boxDecoder
        elif t.IsGenericType then
            if FSharpType.IsTuple(t) then
                let decoders = FSharpType.GetTupleElements(t) |> Array.map (autoDecoder extra isCamelCase false)
                fun path value ->
                    if Helpers.isArray value then
                        mixedArray "tuple elements" decoders path (Helpers.asArray value)
                        |> Result.map (fun xs -> FSharpValue.MakeTuple(List.toArray xs, t))
                    else (path, BadPrimitive ("an array", value)) |> Error
            else
                let fullname = t.GetGenericTypeDefinition().FullName
                if fullname = typedefof<obj option>.FullName then
                    t.GenericTypeArguments.[0] |> (autoDecoder extra isCamelCase true) |> option |> boxDecoder
                elif fullname = typedefof<obj list>.FullName then
                    t.GenericTypeArguments.[0] |> (autoDecoder extra isCamelCase false) |> list |> boxDecoder
                elif fullname = typedefof< Map<string, obj> >.FullName then
                    let keyDecoder = t.GenericTypeArguments.[0] |> autoDecoder extra isCamelCase false
                    let valueDecoder = t.GenericTypeArguments.[1] |> autoDecoder extra isCamelCase false
                    oneOf [
                        autoObject2 keyDecoder valueDecoder
                        list (tuple2 keyDecoder valueDecoder)
                    ] |> map (fun ar -> toMap (unbox ar) |> box)
                elif fullname = typedefof< Set<string> >.FullName then
                    let decoder = t.GenericTypeArguments.[0] |> autoDecoder extra isCamelCase false
                    fun path value ->
                        match array decoder path value with
                        | Error er -> Error er
                        | Ok ar -> toSet (unbox ar) |> box |> Ok
                else
                    autoDecodeRecordsAndUnions extra isCamelCase isOptional t
        else
            if fullname = typeof<bool>.FullName then
                boxDecoder bool
            elif fullname = typeof<string>.FullName then
                boxDecoder string
            elif fullname = typeof<int>.FullName then
                boxDecoder int
            elif fullname = typeof<uint32>.FullName then
                boxDecoder uint32
            elif fullname = typeof<float>.FullName then
                boxDecoder float
            // These number types require extra libraries in Fable. To prevent penalizing
            // all users, extra decoders (withInt64, etc) must be passed when they're needed.

            // elif fullname = typeof<int64>.FullName then
            //     boxDecoder int64
            // elif fullname = typeof<uint64>.FullName then
            //     boxDecoder uint64
            // elif fullname = typeof<bigint>.FullName then
            //     boxDecoder bigint
            // elif fullname = typeof<decimal>.FullName then
            //     boxDecoder decimal
            elif fullname = typeof<System.DateTime>.FullName then
                boxDecoder datetime
            elif fullname = typeof<System.DateTimeOffset>.FullName then
                boxDecoder datetimeOffset
            elif fullname = typeof<System.Guid>.FullName then
                boxDecoder guid
            elif fullname = typeof<obj>.FullName then
                value
            else autoDecodeRecordsAndUnions extra isCamelCase isOptional t

    type Auto =
        static member generateDecoder<'T>(?isCamelCase : bool, ?extra: ExtraDecoders, [<Inject>] ?resolver: ITypeResolver<'T>): Decoder<'T> =
            let isCamelCase = defaultArg isCamelCase false
            let extra = match extra with Some e -> e | None -> makeExtra()
            resolver.Value.ResolveType() |> autoDecoder extra isCamelCase false |> unboxDecoder

        static member fromString<'T>(json: string, ?isCamelCase : bool, ?extra: ExtraDecoders, [<Inject>] ?resolver: ITypeResolver<'T>): Result<'T, string> =
            let decoder = Auto.generateDecoder(?isCamelCase=isCamelCase, ?extra=extra, ?resolver=resolver)
            fromString decoder json

        static member unsafeFromString<'T>(json: string, ?isCamelCase : bool, ?extra: ExtraDecoders, [<Inject>] ?resolver: ITypeResolver<'T>): 'T =
            let decoder = Auto.generateDecoder(?isCamelCase=isCamelCase, ?extra=extra, ?resolver=resolver)
            match fromString decoder json with
            | Ok x -> x
            | Error msg -> failwith msg
