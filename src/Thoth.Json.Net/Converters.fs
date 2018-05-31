module Thoth.Json.Net.Converters

open System
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
        MapConverter() :> JsonConverter
    |]
