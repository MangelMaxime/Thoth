#if INTERACTIVE
#r "../../packages/newtonsoft/Newtonsoft.Json/lib/net45/Newtonsoft.Json.dll"
#else
module Thoth.Json.Net.Converters
#endif

open System
open System.Collections.Concurrent
open System.Reflection
open FSharp.Reflection
open Newtonsoft.Json
open Newtonsoft.Json.Linq

let private advance(reader: JsonReader) =
    reader.Read() |> ignore

/// ATTENTION: The reader should be located in the first element of the array (not in StartArray)
let private readElements(reader: JsonReader, itemTypes: Type[], serializer: JsonSerializer): obj seq =
    let mutable index = 0
    let elems = ResizeArray()
    while reader.TokenType <> JsonToken.EndArray do
        elems.Add(serializer.Deserialize(reader, itemTypes.[index]))
        index <- index + 1
        advance reader
    upcast elems

let private getUci (reader: JsonReader) t name =
    FSharpType.GetUnionCases(t, allowAccessToPrivateRepresentation=true)
    |> Array.tryFind (fun uci -> uci.Name = name)
    |> function
        | Some uci -> uci
        | None -> failwithf "Cannot find case %s in %s (path: %s)" name (string t) reader.Path

let private makeUnion (reader: JsonReader) uci values =
    try FSharpValue.MakeUnion(uci, values, allowAccessToPrivateRepresentation=true)
    with ex -> failwithf "Cannot create union %s (case %s) with %A (path: %s): %s"
                    (string uci.DeclaringType) uci.Name values reader.Path ex.Message

type OptionConverter() =
    inherit JsonConverter()
    override __.CanConvert(t) =
        t.Name = "FSharpOption`1" && t.Namespace = "Microsoft.FSharp.Core"
    override __.WriteJson(writer, value, serializer) =
        let t = value.GetType()
        let _, fields = FSharpValue.GetUnionFields(value, t, allowAccessToPrivateRepresentation=true)
        if fields.Length = 0
        then writer.WriteNull()
        else serializer.Serialize(writer, fields.[0])
    override __.ReadJson(reader, t, _existingValue, serializer) =
        match reader.TokenType with
        | JsonToken.Null -> makeUnion reader (getUci reader t "None") [||]
        | _ -> let value = serializer.Deserialize(reader, t.GenericTypeArguments.[0])
               makeUnion reader (getUci reader t "Some") [|value|]

/// Newtonsoft.Json is capable of converting F# records but it fails if the constructor is private
type RecordConverter() =
    inherit JsonConverter()
    override __.CanConvert(t) =
        FSharpType.IsRecord(t, allowAccessToPrivateRepresentation=true)
    override __.WriteJson(writer, value, serializer) =
        let fieldInfos = FSharpType.GetRecordFields(value.GetType(), allowAccessToPrivateRepresentation=true)
        let fields = FSharpValue.GetRecordFields(value, allowAccessToPrivateRepresentation=true)
        writer.WriteStartObject()
        let useCamelCase = serializer.ContractResolver :? Serialization.CamelCasePropertyNamesContractResolver
        for i=0 to fields.Length - 1 do
            let name = fieldInfos.[i].Name
            let name = if useCamelCase then name.[..0].ToLowerInvariant() + name.[1..] else name
            writer.WritePropertyName(name)
            serializer.Serialize(writer, fields.[i])
        writer.WriteEndObject()
    // TODO: This won't work for records with private constructors, but it's ok
    // because we're using Decode.Auto.fromString which doesn't call the converters
    override __.ReadJson(reader, t, _existingValue, serializer) =
        failwith "Reading is not implemented for RecordConverter"
    override __.CanRead = false

type UnionConverter() =
    inherit JsonConverter()
    override __.CanConvert(t) =
        FSharpType.IsUnion(t, allowAccessToPrivateRepresentation=true)
        && t.Name <> "FSharpList`1"
        && t.Name <> "FSharpOption`1"
    override __.WriteJson(writer, value, serializer) =
        let t = value.GetType()
        let uci, fields = FSharpValue.GetUnionFields(value, t, allowAccessToPrivateRepresentation=true)
        if fields.Length = 0 then
            serializer.Serialize(writer, uci.Name)
        else
            let ar = Array.zeroCreate<obj> (fields.Length + 1)
            ar.[0] <- box uci.Name
            Array.Copy(fields, 0, ar, 1, fields.Length)
            serializer.Serialize(writer, ar)
    override __.ReadJson(reader, t, _existingValue, serializer) =
        match reader.TokenType with
        | JsonToken.String ->
            let name = serializer.Deserialize(reader, typeof<string>) :?> string
            makeUnion reader (getUci reader t name) [||]
        | JsonToken.StartArray ->
            advance reader
            let name =
                match reader.TokenType with
                | JsonToken.String -> reader.Value :?> string
                | token -> failwithf "Expecting string (case name) as first element of array but got %s (path: %s)"
                                        (Enum.GetName(typeof<JsonToken>, token)) reader.Path
            let uci = getUci reader t name
            advance reader
            let itemTypes = uci.GetFields() |> Array.map (fun pi -> pi.PropertyType)
            let values = readElements(reader, itemTypes, serializer) |> Seq.toArray
            makeUnion reader uci values
        | token -> failwithf "Expecting string or array for %s but got %s (path: %s)"
                        (string t) (Enum.GetName(typeof<JsonToken>, token)) reader.Path

type TupleConverter() =
    inherit JsonConverter()
    override __.CanConvert(t) =
        FSharpType.IsTuple t
    override __.WriteJson(writer, value, serializer) =
        let values = FSharpValue.GetTupleFields(value)
        serializer.Serialize(writer, values)
    override __.ReadJson(reader, t, _existingValue, serializer) =
        match reader.TokenType with
        | JsonToken.StartArray ->
            advance reader
            let values = readElements(reader, FSharpType.GetTupleElements(t), serializer) |> Seq.toArray
            try FSharpValue.MakeTuple(values, t)
            with ex -> failwithf "Cannot create tuple %s with %A (path: %s): %s"
                            (string t) values reader.Path ex.Message
        | JsonToken.Null -> null // {"tuple": null}
        | token -> failwithf "Expecting array for tuple got %s (path: %s)"
                        (Enum.GetName(typeof<JsonToken>, token)) reader.Path

type MapConverter() =
    inherit JsonConverter()
    override __.CanConvert(t) =
        t.Name = "FSharpMap`2"
        && t.Namespace = "Microsoft.FSharp.Collections"
    override __.WriteJson(writer, value, serializer) =
        let mutable kvProps: PropertyInfo[] = null
        writer.WriteStartArray()
        for kv in value :?> System.Collections.IEnumerable do
            if isNull kvProps then
                kvProps <- kv.GetType().GetProperties()
            writer.WriteStartArray()
            serializer.Serialize(writer, kvProps.[0].GetValue(kv))
            serializer.Serialize(writer, kvProps.[1].GetValue(kv))
            writer.WriteEndArray()
        writer.WriteEndArray()
    override __.ReadJson(reader, t, _existingValue, serializer) =
        match reader.TokenType with
        | JsonToken.StartArray ->
            let mapTypes = t.GetGenericArguments()
            let tupleType = typedefof<obj*obj>.MakeGenericType(mapTypes)
            let listType = typedefof< ResizeArray<obj> >.MakeGenericType([|tupleType|])
            let list = System.Activator.CreateInstance(listType)
            let meth = listType.GetMethod("Add")
            advance reader
            while reader.TokenType <> JsonToken.EndArray do
                advance reader
                let values = readElements(reader, mapTypes, serializer)
                let tuple = FSharpValue.MakeTuple(Seq.toArray values, tupleType)
                meth.Invoke(list, [|tuple|]) |> ignore
                advance reader
            let cons = t.GetConstructors() |> Array.head
            cons.Invoke([|list|])
        | token -> failwithf "Expecting array for map got %s (path: %s)"
                        (Enum.GetName(typeof<JsonToken>, token)) reader.Path

let All =
    [|
        OptionConverter() :> JsonConverter
        UnionConverter() :> JsonConverter
        RecordConverter() :> JsonConverter
        TupleConverter() :> JsonConverter
        MapConverter() :> JsonConverter
    |]

#if INTERACTIVE
let encodeWithConverter<'T> (converter: JsonConverter) (space: int) (value: obj) : string =
    let format = if space = 0 then Formatting.None else Formatting.Indented
    let settings = JsonSerializerSettings(Converters = [|converter|],
                                          Formatting = format,
                                          DateTimeZoneHandling = DateTimeZoneHandling.Utc )
    JsonConvert.SerializeObject(value, settings)

let decodeStringWithConverter<'T> (converter: JsonConverter) (json: string): Result<'T, string> =
    try
        JsonConvert.DeserializeObject<'T>(json, converter) |> Ok
    with ex ->
        Error("Given an invalid JSON: " + ex.Message)

let equal expected actual =
    let areEqual = expected = actual
    printfn "%A = %A > %b" expected actual areEqual
    if not areEqual then
        failwithf "[ASSERT ERROR] Expected %A but got %A" expected actual

let testCase (msg: string) f: unit =
    try
        printfn "%s" msg
        f ()
    with ex ->
        printfn "%s" ex.Message
        if ex.Message.StartsWith("[ASSERT ERROR]") |> not then
            printfn "%s" ex.StackTrace
    printfn ""

type MyUnion = Foo of int

type Record2 =
    { a : float
      b : float }

type Record9 =
    { a: int
      b: string
      c: (bool * int) list
      d: MyUnion
      e: Map<string, Record2>
      f: DateTime
    }

let converter = CacheConverter(converters)

testCase "Auto.Generate works" <| fun _ ->
    let json =
        { a = 5
          b = "bar"
          c = [false, 3; true, 5; false, 10]
          d = Foo 14
          e = Map [("oh", { a = 2.; b = 2. }); ("ah", { a = -1.5; b = 0. })]
          f = DateTime.Now
        } |> encodeWithConverter converter 4
    printfn "AUTO ENCODED %s" json
    // let decoder = Auto.Generate<Record9>()
    let result = decodeStringWithConverter converter json
    match result with
    | Error er -> failwith er
    | Ok r2 ->
        equal 5 r2.a
        equal "bar" r2.b
        equal [false, 3; true, 5; false, 10] r2.c
        equal (Foo 14) r2.d
        equal -1.5 (Map.find "ah" r2.e).a
        equal 2.   (Map.find "oh" r2.e).b

#endif
