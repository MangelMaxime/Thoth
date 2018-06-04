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

let private advance(reader: JsonReader) =
    reader.Read() |> ignore

let private readElements(reader: JsonReader, itemTypes: Type[], serializer: JsonSerializer): obj seq =
    let mutable index = 0
    let elems = ResizeArray()
    if reader.TokenType = JsonToken.StartArray then
        advance reader
    while reader.TokenType <> JsonToken.EndArray do
        elems.Add(serializer.Deserialize(reader, itemTypes.[index]))
        index <- index + 1
        advance reader
    upcast elems

type UnionConverter() =
    inherit JsonConverter()
    let getUci t name =
        FSharpType.GetUnionCases(t)
        |> Array.find (fun uci -> uci.Name = name)
    override __.CanConvert(t) =
        FSharpType.IsUnion t
        && t.Name <> "FSharpList`1"
    override __.WriteJson(writer, value, serializer) =
        let t = value.GetType()
        let uci, fields = FSharpValue.GetUnionFields(value, t)
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
            FSharpValue.MakeUnion(getUci t name, [||])
        | JsonToken.StartArray ->
            advance reader
            let name = reader.Value :?> string
            let uci = getUci t name
            advance reader
            let itemTypes = uci.GetFields() |> Array.map (fun pi -> pi.PropertyType)
            let values = readElements(reader, itemTypes, serializer)
            FSharpValue.MakeUnion(uci, Seq.toArray values)
        | _ -> failwith "invalid token"

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
            let values = readElements(reader, FSharpType.GetTupleElements(t), serializer)
            FSharpValue.MakeTuple(Seq.toArray values, t)
        | JsonToken.Null -> null // {"tuple": null}
        | _ -> failwith "invalid token"

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
                let values = readElements(reader, mapTypes, serializer)
                let tuple = FSharpValue.MakeTuple(Seq.toArray values, tupleType)
                meth.Invoke(list, [|tuple|]) |> ignore
                advance reader
            let cons = t.GetConstructors() |> Array.head
            cons.Invoke([|list|])
        | _ -> failwith "invalid token"

let converters =
    [|
        UnionConverter() :> JsonConverter
        TupleConverter() :> JsonConverter
        MapConverter() :> JsonConverter
    |]

type CacheConverter(converters: JsonConverter[]) =
    inherit JsonConverter()
    let cache = ConcurrentDictionary<Type, JsonConverter>()
    static let mutable singleton: CacheConverter option = None
    static member Singleton =
        match singleton with
        | Some x -> x
        | None ->
            let x = CacheConverter(converters)
            singleton <- Some x
            x
    override __.CanConvert(t) =
        let conv =
            cache.GetOrAdd(t, fun t ->
                converters
                |> Array.tryFind (fun conv -> conv.CanConvert(t))
                |> Option.toObj)
        not(isNull conv)
    override __.WriteJson(writer, value, serializer) =
        match cache.TryGetValue(value.GetType()) with
        | false, _
        | true, null -> serializer.Serialize(writer, value)
        | true, conv -> conv.WriteJson(writer, value, serializer)
    override __.ReadJson(reader, t, existingValue, serializer) =
        match cache.TryGetValue(t) with
        | false, _
        | true, null -> serializer.Deserialize(reader, t)
        | true, conv -> conv.ReadJson(reader, t, existingValue, serializer)

#if INTERACTIVE
let encodeWithConverter<'T> (converter: JsonConverter) (space: int) (value: obj) : string =
    // TODO: Can we set indentation space?
    let format = if space = 0 then Formatting.None else Formatting.Indented
    let settings = JsonSerializerSettings(Converters = [|converter|],
                                          Formatting = format)
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
