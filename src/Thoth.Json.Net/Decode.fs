namespace Thoth.Json.Net

[<RequireQualifiedAccess>]
module Decode =

    open System.Globalization
    open Newtonsoft.Json
    open Newtonsoft.Json.Linq
    open System.IO

    type ErrorReason =
        | BadPrimitive of string * JToken
        | BadPrimitiveExtra of string * JToken * string
        | BadField of string * JToken
        | BadType of string * JToken
        | BadTypeAt of string * JToken
        | BadPath of string * JToken * string
        | TooSmallArray of string * JToken
        | FailMessage of string
        | BadOneOf of string list

    type DecoderError = string * ErrorReason

    type Decoder<'T> = string -> JToken -> Result<'T, DecoderError>

    module private Helpers =
        let anyToString (token: JToken) : string =
            use stream = new StringWriter(NewLine = "\n")
            use jsonWriter = new JsonTextWriter(
                                    stream,
                                    Formatting = Formatting.Indented,
                                    Indentation = 4 )

            token.WriteTo(jsonWriter)
            stream.ToString()

        let inline isBool (token: JToken) = token.Type = JTokenType.Boolean
        let inline isNumber (token: JToken) = token.Type = JTokenType.Float || token.Type = JTokenType.Integer
        let inline isString (token: JToken) = token.Type = JTokenType.String
        let inline isArray (token: JToken) = token.Type = JTokenType.Array
        let inline isObject (token: JToken) = token.Type = JTokenType.Object
        let inline isNull (token: JToken) = token.Type = JTokenType.Null
        let inline asBool (token: JToken): bool = token.Value<bool>()
        let inline asInt (token: JToken): int = token.Value<int>()
        let inline asFloat (token: JToken): float = token.Value<float>()
        let inline asDecimal (token: JToken): System.Decimal = token.Value<System.Decimal>()
        let inline asString (token: JToken): string = token.Value<string>()
        let inline asArray (token: JToken): JToken[] = token.Value<JArray>().Values() |> Seq.toArray

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

    let unwrap (path : string) (decoder : Decoder<'T>) (value : JToken) : 'T =
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
                let json = Newtonsoft.Json.Linq.JValue.Parse value
                fromValue "$" decoder json
            with
                | :? Newtonsoft.Json.JsonReaderException as ex ->
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
        fun path token ->
            if Helpers.isString token then
                Ok(Helpers.asString token)
            else
                (path, BadPrimitive("a string", token)) |> Error

    let guid : Decoder<System.Guid> =
        fun path value ->
            // Using Helpers.isString fails because Json.NET directly assigns Guid type
            if value.Type = JTokenType.Guid then
                value.Value<System.Guid>() |> Ok
            elif value.Type = JTokenType.String then
                match System.Guid.TryParse (Helpers.asString value) with
                | true, x -> Ok x
                | _ -> (path, BadPrimitive("a guid", value)) |> Error
            else (path, BadPrimitive("a guid", value)) |> Error

    let int : Decoder<int> =
        fun path token ->
            if token.Type = JTokenType.Integer then
                let value = token.Value<decimal> ()
                if value >= (decimal System.Int32.MinValue) && value <= (decimal System.Int32.MaxValue) then
                    Ok (int32 value)
                else
                    (path, BadPrimitiveExtra("an int", token, "Value was either too large or too small for an int")) |> Error
            elif token.Type = JTokenType.String then
                match System.Int32.TryParse (Helpers.asString token, NumberStyles.Any, CultureInfo.InvariantCulture) with
                | true, x -> Ok x
                | _ -> (path, BadPrimitive("an int", token)) |> Error
            else
                (path, BadPrimitive("an int", token)) |> Error

    let int64 : Decoder<int64> =
        fun path token ->
            if token.Type = JTokenType.Integer then
                Ok(token.Value<int64>())
            elif token.Type = JTokenType.String then
                match System.Int64.TryParse (Helpers.asString token, NumberStyles.Any, CultureInfo.InvariantCulture) with
                | true, x -> Ok x
                | _ -> (path, BadPrimitive("an int64", token)) |> Error
            else (path, BadPrimitive("an int64", token)) |> Error

    let uint32 : Decoder<uint32> =
        fun path token ->
            if token.Type = JTokenType.Integer then
                let value = token.Value<decimal> ()
                if value >= 0m && value <= (decimal System.UInt32.MaxValue) then
                    Ok (uint32 value)
                else
                    (path, BadPrimitiveExtra("an uint32", token, "Value was either too large or too small for an uint32")) |> Error
            elif token.Type = JTokenType.String then
                match System.UInt32.TryParse (Helpers.asString token, NumberStyles.Any, CultureInfo.InvariantCulture) with
                | true, x -> Ok x
                | _ -> (path, BadPrimitive("an uint32", token)) |> Error
            else (path, BadPrimitive("an uint32", token)) |> Error

    let uint64 : Decoder<uint64> =
        fun path token ->
            if token.Type = JTokenType.Integer then
                let value = token.Value<decimal>()
                if value >= 0m && value <= (decimal System.UInt64.MaxValue) then
                    Ok (uint64 value)
                else
                    (path, BadPrimitiveExtra("an uint64", token, "Value was either too large or too small for an uint64")) |> Error
            elif token.Type = JTokenType.String then
                match System.UInt64.TryParse (Helpers.asString token, NumberStyles.Any, CultureInfo.InvariantCulture) with
                | true, x -> Ok x
                | _ -> (path, BadPrimitive("an uint64", token)) |> Error
            else (path, BadPrimitive("an uint64", token)) |> Error

    let bigint : Decoder<bigint> =
        fun path token ->
            if Helpers.isNumber token then
                Helpers.asInt token |> bigint |> Ok
            elif Helpers.isString token then
                match bigint.TryParse (Helpers.asString token, NumberStyles.Any, CultureInfo.InvariantCulture) with
                | true, x -> Ok x
                | _ -> (path, BadPrimitive("a bigint", token)) |> Error
            else
                (path, BadPrimitive("a bigint", token)) |> Error

    let bool : Decoder<bool> =
        fun path token ->
            if Helpers.isBool token then
                Ok(Helpers.asBool token)
            else
                (path, BadPrimitive("a boolean", token)) |> Error

    let float : Decoder<float> =
        fun path token ->
            if Helpers.isNumber token then
                Helpers.asFloat token |> Ok
            else
                (path, BadPrimitive("a float", token)) |> Error

    let decimal : Decoder<decimal> =
        fun path token ->
            if Helpers.isNumber token then
                Helpers.asDecimal token |> Ok
            elif Helpers.isString token then
                match System.Decimal.TryParse (Helpers.asString token, NumberStyles.Any, CultureInfo.InvariantCulture) with
                | true, x -> Ok x
                | _ -> (path, BadPrimitive("a decimal", token)) |> Error
            else
                (path, BadPrimitive("a decimal", token)) |> Error

    let datetime : Decoder<System.DateTime> =
        fun path token ->
            if token.Type = JTokenType.Date || token.Type = JTokenType.String then
                match System.DateTime.TryParse (Helpers.asString token, CultureInfo.InvariantCulture, DateTimeStyles.None) with
                | true, x -> Ok x
                | _ -> (path, BadPrimitive("a datetime", token)) |> Error
            else
                (path, BadPrimitive("a datetime", token)) |> Error

    let datetimeOffset : Decoder<System.DateTimeOffset> =
        fun path token ->
            if token.Type = JTokenType.Date || token.Type = JTokenType.String then
                match System.DateTimeOffset.TryParse (Helpers.asString token, CultureInfo.InvariantCulture, DateTimeStyles.None) with
                | true, x -> Ok x
                | _ -> (path, BadPrimitive("a datetimeoffset", token)) |> Error
            else
                (path, BadPrimitive("a datetimeoffset", token)) |> Error

    /////////////////////////
    // Object primitives ///
    ///////////////////////

    let field (fieldName: string) (decoder : Decoder<'value>) : Decoder<'value> =
        fun path token ->
            let currentPath = path + "." + fieldName
            if token.Type = JTokenType.Object then
                let fieldValue = token.Item(fieldName)
                if isNull fieldValue then
                    (currentPath, BadField ("an object with a field named `" + fieldName + "`", token)) |> Error
                else
                    decoder currentPath fieldValue
            else
                (currentPath, BadType("an object", token))
                |> Error

    exception UndefinedValueException of string
    exception NonObjectTypeException

    let at (fieldNames: string list) (decoder : Decoder<'value>) : Decoder<'value> =
        fun path token ->
            let mutable cValue = token
            let mutable currentPath = path
            let mutable index = 0
            try
                for fieldName in fieldNames do
                    if cValue.Type = JTokenType.Object then
                        let currentNode = cValue.Item(fieldName)
                        currentPath <- currentPath + "." + fieldName
                        if isNull currentNode then
                            raise (UndefinedValueException fieldName)
                        else
                            cValue <- currentNode
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
                    (currentPath, BadPath (msg, token, fieldName))
                    |> Error

    let index (requestedIndex: int) (decoder : Decoder<'value>) : Decoder<'value> =
        fun path token ->
            let currentPath = path + ".[" + (Operators.string requestedIndex) + "]"
            if token.Type = JTokenType.Array then
                let vArray = Helpers.asArray token
                if requestedIndex < vArray.Length then
                    unwrap currentPath decoder (vArray.[requestedIndex]) |> Ok
                else
                    let msg =
                        "a longer array. Need index `"
                            + (requestedIndex.ToString())
                            + "` but there are only `"
                            + (vArray.Length.ToString())
                            + "` entries"

                    (currentPath, TooSmallArray(msg, token))
                    |> Error
            else
                (currentPath, BadPrimitive("an array", token))
                |> Error


    let optional (fieldName : string) (decoder : Decoder<'value>) : Decoder<'value option> =
        fun path v ->
            match decodeValueError path (field fieldName decoder) v with
            | Ok v -> Ok (Some v)
            | Error (_, BadField _ ) -> Ok None
            | Error (_, BadType (_, jToken))
            | Error (_, BadPrimitive (_, jToken)) when jToken.Type = JTokenType.Null -> Ok None
            | Error error ->
                raise (DecoderException error)

    let optionalAt (fieldNames : string list) (decoder : Decoder<'value>) : Decoder<'value option> =
        fun path v ->
            match decodeValueError path (at fieldNames decoder) v with
            | Ok v -> Ok (Some v)
            | Error (_, BadPath _ )
            | Error (_, BadTypeAt _) -> Ok None
            | Error (_, BadType (_, jToken))
            | Error (_, BadPrimitive (_, jToken)) when jToken.Type = JTokenType.Null -> Ok None
            | Error error ->
                raise (DecoderException error)

    //////////////////////
    // Data structure ///
    ////////////////////

    let private listPrivate (decoder : Decoder<'value>) path token =
        let tokens = Helpers.asArray token
        let mutable i = tokens.Length
        (tokens, Ok []) ||> Array.foldBack (fun value acc ->
            i <- i - 1
            match acc with
            | Error _ -> acc
            | Ok acc ->
                match decoder (path + "[" + (i.ToString()) + "]") value with
                | Error er -> Error er
                | Ok value -> Ok (value::acc))

    let list (decoder : Decoder<'value>) : Decoder<'value list> =
        fun path token ->
            if token.Type = JTokenType.Array then
                let mutable i = -1
                let tokens = Helpers.asArray token
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
                (path, BadPrimitive ("a list", token))
                |> Error

    let array (decoder : Decoder<'value>) : Decoder<'value array> =
        fun path token ->
            if token.Type = JTokenType.Array then
                let mutable i = -1
                let tokens = Helpers.asArray token
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
                (path, BadPrimitive ("an array", token))
                |> Error

    let keyValuePairs (decoder : Decoder<'value>) : Decoder<(string * 'value) list> =
        fun path token ->
            if token.Type = JTokenType.Object then
                let value = token.Value<JObject>()

                value.Properties()
                |> Seq.map (fun prop ->
                    (prop.Name, value.Item(prop.Name) |> unwrap path decoder)
                )
                |> Seq.toList
                |> Ok
            else
                (path, BadPrimitive ("an object", token))
                |> Error

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
                | Error (_, BadField _ ) -> Ok None
                | Error (_, BadType (_, jToken))
                | Error (_, BadPrimitive (_, jToken)) when jToken.Type = JTokenType.Null -> Ok None
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
        fun path token ->
            if token.Type = JTokenType.Null then
                Ok output
            else
                (path, BadPrimitive("null", token)) |> Error

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
                            | Error (_, BadField _ ) -> None
                            | Error (_, BadType (_, jToken))
                            | Error (_, BadPrimitive (_, jToken)) when jToken.Type = JTokenType.Null -> None
                            | Error error ->
                                raise (DecoderException error)
                        member __.At (fieldNames : string list) (decoder : Decoder<_>) =
                            if Helpers.isObject v then
                                match decodeValueError path (at fieldNames decoder) v with
                                | Ok v -> Some v
                                | Error (_, BadPath _ )
                                | Error (_, BadTypeAt _) -> None
                                | Error (_, BadType (_, jToken))
                                | Error (_, BadPrimitive (_, jToken)) when jToken.Type = JTokenType.Null -> None
                                | Error error ->
                                    raise (DecoderException error)
                            else
                                raise (DecoderException (path, BadType ("an object", v)))
                        member __.Raw (decoder: Decoder<_>) =
                            match decodeValueError path decoder v with
                            | Ok v -> Some v
                            | Error (_, BadField _ ) -> None
                            | Error (_, BadPrimitive (_, jToken)) when jToken.Type = JTokenType.Null -> None
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
        | Optional of System.Type
        | Required

        member this.ToBool
            with get () =
                match this with
                | Optional _ -> true
                | Required -> false

    [<AbstractClass>]
    type private BoxedDecoder() =
        abstract Decode: path : string * token: JToken -> Result<obj, DecoderError>
        member this.BoxedDecoder: Decoder<obj> =
            fun path token -> this.Decode(path, token)

    type private DecoderCrate<'T>(dec: Decoder<'T>) =
        inherit BoxedDecoder()
        override __.Decode(path, token) =
            match dec path token with
            | Ok v -> Ok(box v)
            | Error er -> Error er
        member __.UnboxedDecoder = dec

    let inline private boxDecoder (d: Decoder<'T>): BoxedDecoder =
        DecoderCrate(d) :> BoxedDecoder

    let inline private unboxDecoder<'T> (d: BoxedDecoder): Decoder<'T> =
        (d :?> DecoderCrate<'T>).UnboxedDecoder

    let private autoObject (decoderInfos: (FieldType * string * BoxedDecoder)[]) (path : string) (value: JToken) =
        if not (Helpers.isObject value) then
            (path, BadPrimitive ("an object", value)) |> Error
        else
            (decoderInfos, Ok []) ||> Array.foldBack (fun (fieldType, name, decoder) acc ->
                match acc with
                | Error _ -> acc
                | Ok result ->
                    match fieldType with
                    | FieldType.Optional typ ->
                        optional name decoder.BoxedDecoder path value
                        |> Result.map (fun v ->
                            match v with
                            | Some v ->
                                let ucis = FSharpType.GetUnionCases(typ)
                                (FSharpValue.MakeUnion(ucis.[1], [|v|]))::result
                            | None -> box None::result
                        )
                    | FieldType.Required ->
                        field name decoder.BoxedDecoder path value
                        |> Result.map (fun v -> v::result))

    let private mixedArray msg (decoders: BoxedDecoder[]) (path: string) (values: JToken[]): Result<obj list, DecoderError> =
        if decoders.Length <> values.Length then
            (path, sprintf "Expected %i %s but got %i" decoders.Length msg values.Length
            |> FailMessage) |> Error
        else
            (values, decoders, Ok [])
            |||> Array.foldBack2 (fun value decoder acc ->
                match acc with
                | Error _ -> acc
                | Ok result -> decoder.Decode(path, value) |> Result.map (fun v -> v::result))

    let private genericOption t (decoder: BoxedDecoder) =
        fun (path : string)  (value: JToken) ->
            // TODO: Is GetUnionCases a costly operation? Should we cache this?
            let ucis = FSharpType.GetUnionCases(t)
            if Helpers.isNull value then
                box None |> Ok
            else
                match decoder.Decode(path, value) with
                | Ok v -> Ok (FSharpValue.MakeUnion(ucis.[1], [|v|]))
                | Error (_, BadField _ ) ->
                    box None |> Ok
                | Error (_, BadType (_, jToken))
                | Error (_, BadPrimitive (_, jToken)) when jToken.Type = JTokenType.Null ->
                    box None |> Ok
                | Error error -> Error error

    let private genericList t (decoder: BoxedDecoder) =
        fun (path : string)  (value: JToken) ->
            if not (Helpers.isArray value) then
                (path, BadPrimitive ("a list", value)) |> Error
            else
                let values = value.Value<JArray>()
                let ucis = FSharpType.GetUnionCases(t)
                let empty = FSharpValue.MakeUnion(ucis.[0], [||])
                (values, Ok empty) ||> Seq.foldBack (fun value acc ->
                    match acc with
                    | Error _ -> acc
                    | Ok acc ->
                        match decoder.Decode(path, value) with
                        | Error er -> Error er
                        | Ok result -> FSharpValue.MakeUnion(ucis.[1], [|result; acc|]) |> Ok)

    let rec private genericMap isCamelCase (t: System.Type) =
        let keyType   = t.GenericTypeArguments.[0]
        let valueType = t.GenericTypeArguments.[1]
        let keyDecoder   = autoDecoder isCamelCase false keyType
        let valueDecoder = autoDecoder isCamelCase false valueType
        let tupleType = typedefof<obj * obj>.MakeGenericType([|keyType; valueType|])
        let listType = typedefof< ResizeArray<obj> >.MakeGenericType([|tupleType|])
        let addMethod = listType.GetMethod("Add")
        fun (path : string)  (value: JToken) ->
            if not (Helpers.isArray value) then
                (path, BadPrimitive ("an array", value)) |> Error
            else
                let values = value.Value<JArray>()
                let empty = System.Activator.CreateInstance(listType)
                (Ok empty, values) ||> Seq.fold (fun acc value ->
                    match acc with
                    | Error _ -> acc
                    | Ok acc ->
                        if not (Helpers.isArray value) then
                            (path, BadPrimitive ("an array", value)) |> Error
                        else
                            let kv = value.Value<JArray>()
                            match keyDecoder.Decode(path + "[0]", kv.[0]), valueDecoder.Decode(path + "[1]", kv.[1]) with
                            | Error er, _ -> Error er
                            | _, Error er -> Error er
                            | Ok key, Ok value ->
                                addMethod.Invoke(acc, [|FSharpValue.MakeTuple([|key; value|], tupleType)|]) |> ignore
                                Ok acc)
                |> function
                    | Error er -> Error er
                    | Ok kvs ->
                        let mapType = typedefof< Map<string, obj> >.MakeGenericType([|keyType; valueType|])
                        System.Activator.CreateInstance(mapType, kvs) |> Ok

    and private makeUnion t isCamelCase name (path : string) (values: JToken[]) =
        match FSharpType.GetUnionCases(t) |> Array.tryFind (fun x -> x.Name = name) with
        | None -> (path, FailMessage("Cannot find case " + name + " in " + t.FullName)) |> Error
        | Some uci ->
            if values.Length = 0 then
                FSharpValue.MakeUnion(uci, [||]) |> Ok
            else
                let decoders = uci.GetFields() |> Array.map (fun fi -> autoDecoder isCamelCase false fi.PropertyType)
                mixedArray "union fields" decoders path values
                |> Result.map (fun values -> FSharpValue.MakeUnion(uci, List.toArray values))

    and private autoDecodeRecordsAndUnions (t: System.Type) (isCamelCase : bool) (isOptional : bool) : BoxedDecoder =
        if FSharpType.IsRecord(t) then
            boxDecoder(fun path value ->
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
                                    FieldType.Optional fi.PropertyType, fi.PropertyType.GenericTypeArguments.[0]
                                else
                                    FieldType.Required, fi.PropertyType
                            else
                                FieldType.Required, fi.PropertyType

                        fieldType, name, autoDecoder isCamelCase fieldType.ToBool propertyType)
                autoObject decoders path value
                |> Result.map (fun xs -> FSharpValue.MakeRecord(t, List.toArray xs)))

        elif FSharpType.IsUnion(t) then
            boxDecoder(fun path (value: JToken) ->
                if Helpers.isString(value) then
                    let name = Helpers.asString value
                    makeUnion t isCamelCase name path [||]
                elif Helpers.isArray(value) then
                    let values = Helpers.asArray value
                    let name = Helpers.asString values.[0]
                    makeUnion t isCamelCase name path values.[1..]
                else (path, BadPrimitive("a string or array", value)) |> Error)
        else
            boxDecoder(fun path _ ->
                if isOptional then
                    (path, BadPrimitive ("Generating an error message as the field is optional so the `option` decoders will return `None` instead of failing", JValue.CreateNull()))
                    |> Error
                else
                    (path, sprintf "Class types cannot be automatically deserialized: %s" t.FullName
                    |> FailMessage) |> Error
            )


    and private autoDecoder isCamelCase (isOptional : bool) (t: System.Type) : BoxedDecoder =
        if t.IsArray then
            let elemType = t.GetElementType()
            let decoder = autoDecoder isCamelCase false elemType
            boxDecoder(fun path value ->
                match array decoder.BoxedDecoder path value with
                | Ok items ->
                    let ar = System.Array.CreateInstance(elemType, items.Length)
                    for i = 0 to ar.Length - 1 do
                        ar.SetValue(items.[i], i)
                    Ok ar
                | Error er -> Error er)
        elif t.IsGenericType then
            if FSharpType.IsTuple(t) then
                let decoders = FSharpType.GetTupleElements(t) |> Array.map (autoDecoder isCamelCase false)
                boxDecoder(fun path value ->
                    if Helpers.isArray value then
                        mixedArray "tuple elements" decoders path (Helpers.asArray value)
                        |> Result.map (fun xs -> FSharpValue.MakeTuple(List.toArray xs, t))
                    else (path, BadPrimitive ("an array", value)) |> Error)
            else
                let fullname = t.GetGenericTypeDefinition().FullName
                if fullname = typedefof<obj option>.FullName then
                    autoDecoder isCamelCase true t.GenericTypeArguments.[0] |> genericOption t |> boxDecoder
                elif fullname = typedefof<obj list>.FullName then
                    autoDecoder isCamelCase false t.GenericTypeArguments.[0] |> genericList t |> boxDecoder
                elif fullname = typedefof< Map<string, obj> >.FullName then
                    genericMap isCamelCase t |> boxDecoder
                elif fullname = typedefof< Set<string> >.FullName then
                    let t = t.GenericTypeArguments.[0]
                    let decoder = autoDecoder isCamelCase false t
                    boxDecoder(fun path value ->
                        match array decoder.BoxedDecoder path value with
                        | Ok items ->
                            let ar = System.Array.CreateInstance(t, items.Length)
                            for i = 0 to ar.Length - 1 do
                                ar.SetValue(items.[i], i)
                            let setType = typedefof< Set<string> >.MakeGenericType([|t|])
                            System.Activator.CreateInstance(setType, ar) |> Ok
                        | Error er -> Error er)
                else
                    autoDecodeRecordsAndUnions t isCamelCase isOptional
        else
            let fullname = t.FullName
            if fullname = typeof<bool>.FullName then
                boxDecoder bool
            elif fullname = typeof<string>.FullName then
                boxDecoder string
            elif fullname = typeof<int>.FullName then
                boxDecoder int
            elif fullname = typeof<float>.FullName then
                boxDecoder float
            elif fullname = typeof<decimal>.FullName then
                boxDecoder decimal
            elif fullname = typeof<int64>.FullName then
                boxDecoder int64
            elif fullname = typeof<uint32>.FullName then
                boxDecoder uint32
            elif fullname = typeof<uint64>.FullName then
                boxDecoder uint64
            elif fullname = typeof<bigint>.FullName then
                boxDecoder bigint
            elif fullname = typeof<System.DateTime>.FullName then
                boxDecoder datetime
            elif fullname = typeof<System.DateTimeOffset>.FullName then
                boxDecoder datetimeOffset
            elif fullname = typeof<System.Guid>.FullName then
                boxDecoder guid
            elif fullname = typeof<obj>.FullName then
                boxDecoder (fun _ v ->
                if v.Type = JTokenType.Null then
                    Ok null
                else
                    Ok v)
            else autoDecodeRecordsAndUnions t isCamelCase isOptional

    type Auto =
        static member generateDecoder<'T> (?isCamelCase : bool): Decoder<'T> =
            let decoderCrate = autoDecoder (defaultArg isCamelCase false) false typeof<'T>
            fun path token ->
                match decoderCrate.Decode(path, token) with
                | Ok x -> Ok(x :?> 'T)
                | Error er -> Error er

        static member fromString<'T>(json: string, ?isCamelCase : bool): Result<'T, string> =
            let decoder = Auto.generateDecoder(?isCamelCase=isCamelCase)
            fromString decoder json

        static member unsafeFromString<'T>(json: string, ?isCamelCase : bool): 'T =
            let decoder = Auto.generateDecoder(?isCamelCase=isCamelCase)
            match fromString decoder json with
            | Ok x -> x
            | Error msg -> failwith msg
