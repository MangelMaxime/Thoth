namespace Thoth.Json

[<RequireQualifiedAccess>]
module Encode =

    open System.Collections.Generic
    open System.Globalization
    open Fable.Import
    open Fable.Core
    open Fable.Core.JsInterop

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
        box (value.ToString())

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

    let inline uint32 (value : uint32) : Value =
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
        value.ToString() |> string

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
    let inline array (values : Value array) : Value =
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

    let inline seq (values : Value seq) : Value =
        box (JS.Array.from(values :?> JS.Iterable<Value>))

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
        value.ToString("O", CultureInfo.InvariantCulture) |> string

    let int64 (value : int64) : Value =
        box (value.ToString(CultureInfo.InvariantCulture))

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
        value.ToString("O", CultureInfo.InvariantCulture) |> string

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

    //////////////////
    // Reflection ///
    ////////////////

    open FSharp.Reflection
    open Fable.Core.DynamicExtensions

    // As generics are erased by Fable, let's just do an unsafe cast for performance
    let inline boxEncoder (d: Encoder<'T>): BoxedEncoder =
        !!d

    let inline unboxEncoder (d: BoxedEncoder): Encoder<'T> =
        !!d

    let rec private autoEncodeRecordsAndUnions extra (isCamelCase : bool) (t: System.Type) : BoxedEncoder =
        if FSharpType.IsRecord(t, allowAccessToPrivateRepresentation=true) then
            let setters =
                FSharpType.GetRecordFields(t, allowAccessToPrivateRepresentation=true)
                |> Array.map (fun fi ->
                    let targetKey =
                        if isCamelCase then fi.Name.[..0].ToLowerInvariant() + fi.Name.[1..]
                        else fi.Name
                    let encode = autoEncoder extra isCamelCase fi.PropertyType
                    fun (source: obj) (target: Value) ->
                        let value = FSharpValue.GetRecordField(source, fi)
                        if not(isNull value) then // Discard null fields
                            target.[targetKey] <- encode value
                        target)
            fun (source: obj) ->
                (Value(), setters) ||> Seq.fold (fun target set -> set source target)
        elif FSharpType.IsUnion(t, allowAccessToPrivateRepresentation=true) then
            fun (value: obj) ->
                let info, fields = FSharpValue.GetUnionFields(value, t, allowAccessToPrivateRepresentation=true)
                match fields.Length with
                | 0 -> string info.Name
                | len ->
                    let fieldTypes = info.GetFields()
                    let target = Array.zeroCreate<Value> (len + 1)
                    target.[0] <- string info.Name
                    for i = 1 to len do
                        let encode = autoEncoder extra isCamelCase fieldTypes.[i-1].PropertyType
                        target.[i] <- encode fields.[i-1]
                    array target
        else
            failwithf "Cannot generate auto encoder for %s. Please pass an extra encoder." t.FullName

    and private autoEncoder (extra: ExtraCoders) isCamelCase (t: System.Type) : BoxedEncoder =
      let fullname = t.FullName
      match Map.tryFind fullname extra with
      | Some(encoder,_) -> encoder
      | None ->
        if t.IsArray then
            let encoder = t.GetElementType() |> autoEncoder extra isCamelCase
            fun (value: obj) ->
                value :?> obj seq |> Seq.map encoder |> seq
        elif t.IsGenericType then
            if FSharpType.IsTuple(t) then
                let encoders =
                    FSharpType.GetTupleElements(t)
                    |> Array.map (autoEncoder extra isCamelCase)
                fun (value: obj) ->
                    FSharpValue.GetTupleFields(value)
                    |> Seq.mapi (fun i x -> encoders.[i] x) |> seq
            else
                let fullname = t.GetGenericTypeDefinition().FullName
                if fullname = typedefof<obj option>.FullName then
                    t.GenericTypeArguments.[0] |> autoEncoder extra isCamelCase |> option |> boxEncoder
                elif fullname = typedefof<obj list>.FullName
                    || fullname = typedefof<Set<string>>.FullName then
                    let encoder = t.GenericTypeArguments.[0] |> autoEncoder extra isCamelCase
                    fun (value: obj) ->
                        value :?> obj seq |> Seq.map encoder |> seq
                elif fullname = typedefof< Map<string, obj> >.FullName then
                    let keyType = t.GenericTypeArguments.[0]
                    let valueEncoder = t.GenericTypeArguments.[1] |> autoEncoder extra isCamelCase
                    if keyType.FullName = typeof<string>.FullName
                        || keyType.FullName = typeof<System.Guid>.FullName then
                        fun value ->
                            // Fable compiles Guids as strings so this works, but maybe we should make the conversion explicit
                            // (see dotnet version) in case Fable implementation of Guids change
                            (Value(), value :?> Map<string, obj>)
                            ||> Seq.fold (fun target (KeyValue(k,v)) ->
                                target.[k] <- valueEncoder v
                                target)
                    else
                        let keyEncoder = keyType |> autoEncoder extra isCamelCase
                        fun value ->
                            value :?> Map<string, obj> |> Seq.map (fun (KeyValue(k,v)) ->
                                array [|keyEncoder k; valueEncoder v|]) |> seq
                else
                    autoEncodeRecordsAndUnions extra isCamelCase t
        else
            if fullname = typeof<bool>.FullName then
                boxEncoder bool
            elif fullname = typeof<string>.FullName then
                boxEncoder string
            elif fullname = typeof<int>.FullName then
                boxEncoder int
            elif fullname = typeof<uint32>.FullName then
                boxEncoder uint32
            elif fullname = typeof<float>.FullName then
                boxEncoder float
            // These number types require extra libraries in Fable. To prevent penalizing
            // all users, extra encoders (withInt64, etc) must be passed when they're needed.

            // elif fullname = typeof<int64>.FullName then
            //     boxEncoder int64
            // elif fullname = typeof<uint64>.FullName then
            //     boxEncoder uint64
            // elif fullname = typeof<bigint>.FullName then
            //     boxEncoder bigint
            // elif fullname = typeof<decimal>.FullName then
            //     boxEncoder decimal
            elif fullname = typeof<System.DateTime>.FullName then
                boxEncoder datetime
            elif fullname = typeof<System.DateTimeOffset>.FullName then
                boxEncoder datetimeOffset
            elif fullname = typeof<System.Guid>.FullName then
                boxEncoder guid
            elif fullname = typeof<obj>.FullName then
                boxEncoder id
            else
                autoEncodeRecordsAndUnions extra isCamelCase t

    type Auto =
        static member generateEncoder<'T>(?isCamelCase : bool, ?extra: ExtraCoders, [<Inject>] ?resolver: ITypeResolver<'T>): Encoder<'T> =
            let isCamelCase = defaultArg isCamelCase false
            let extra = match extra with Some e -> e | None -> Map.empty
            resolver.Value.ResolveType() |> autoEncoder extra isCamelCase |> unboxEncoder

        static member toString(space : int, value : 'T, ?isCamelCase : bool, ?extra: ExtraCoders, [<Inject>] ?resolver: ITypeResolver<'T>) : string =
            let encoder = Auto.generateEncoder(?isCamelCase=isCamelCase, ?extra=extra, ?resolver=resolver)
            encoder value |> toString space

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
