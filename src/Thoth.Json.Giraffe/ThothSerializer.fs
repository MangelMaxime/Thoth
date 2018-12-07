namespace Thoth.Json.Giraffe

open System.IO
open FSharp.Control.Tasks.V2
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open Giraffe.Serialization.Json
open Thoth.Json.Net
open System.Text
open System.Threading.Tasks

type ThothSerializer (?isCamelCase : bool, ?extra: ExtraCoders) =
    let Utf8EncodingWithoutBom = new UTF8Encoding(false)
    let DefaultBufferSize = 1024

    interface IJsonSerializer with
        member __.SerializeToString (o : 'T) =
            let encoder = Encode.Auto.generateEncoderCached<'T>(?isCamelCase=isCamelCase, ?extra=extra)
            encoder o |> Encode.toString 0

        member __.Deserialize<'T> (json : string) =
            let decoder = Decode.Auto.generateDecoderCached<'T>(?isCamelCase=isCamelCase, ?extra=extra)
            match Decode.fromString decoder json with
            | Ok x -> x
            | Error er -> failwith er

        member this.Deserialize<'T> (bytes : byte[]) =
            (this :> IJsonSerializer).Deserialize<'T>(Encoding.UTF8.GetString bytes)

        member __.DeserializeAsync<'T> (stream : Stream) = task {
            let decoder = Decode.Auto.generateDecoderCached<'T>(?isCamelCase=isCamelCase, ?extra=extra)
            use streamReader = new System.IO.StreamReader(stream)
            use jsonReader = new JsonTextReader(streamReader)
            let! json = JValue.ReadFromAsync jsonReader
            return
              match Decode.fromValue "$" decoder json with
              | Ok value -> value
              | Error er -> failwith er
          }

        member this.SerializeToBytes<'T>(o : 'T) : byte array =
            (this :> IJsonSerializer).SerializeToString<'T>(o)
            |> Encoding.UTF8.GetBytes

        member __.SerializeToStreamAsync (o : 'T) (stream : Stream) =
            upcast task {
                use streamWriter = new System.IO.StreamWriter(stream, Utf8EncodingWithoutBom, DefaultBufferSize, true)
                use jsonWriter = new JsonTextWriter(streamWriter)
                if typedefof<'T> = typedefof<obj seq> then
                    let t = typeof<'T>.GetGenericArguments().[0]
                    let encoder = Encode.Auto.generateEncoderCached(t, ?isCamelCase=isCamelCase, ?extra=extra)
                    jsonWriter.WriteStartArray()
                    let enumerator = (box o :?> System.Collections.IEnumerable).GetEnumerator()
                    while enumerator.MoveNext() do
                        do! (encoder enumerator.Current).WriteToAsync(jsonWriter)
                    jsonWriter.WriteEndArray()
                else
                    let encoder = Encode.Auto.generateEncoderCached<'T>(?isCamelCase=isCamelCase, ?extra=extra)
                    do! (encoder o).WriteToAsync(jsonWriter)
            }
