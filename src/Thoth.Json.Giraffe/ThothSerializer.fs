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
            let t = o.GetType()
            let encoder = Encode.Auto.generateEncoderCached(t, ?isCamelCase=isCamelCase, ?extra=extra)
            encoder o |> Encode.toString 0

        member __.Deserialize<'T> (json : string) =
            let decoder = Decode.Auto.generateDecoderCached<'T>(?isCamelCase=isCamelCase, ?extra=extra)
            match Decode.fromString decoder json with
            | Ok x -> x
            | Error er -> failwith er

        member __.Deserialize<'T> (bytes : byte[]) =
            let decoder = Decode.Auto.generateDecoderCached<'T>(?isCamelCase=isCamelCase, ?extra=extra)
            use stream = new MemoryStream(bytes)
            use streamReader = new StreamReader(stream)
            use jsonReader = new JsonTextReader(streamReader)
            let json = JValue.ReadFrom jsonReader
            match Decode.fromValue "$" decoder json with
            | Ok value -> value
            | Error er -> failwith er

        member __.DeserializeAsync<'T> (stream : Stream) = task {
            let decoder = Decode.Auto.generateDecoderCached<'T>(?isCamelCase=isCamelCase, ?extra=extra)
            use streamReader = new StreamReader(stream)
            use jsonReader = new JsonTextReader(streamReader)
            let! json = JValue.ReadFromAsync jsonReader
            return
              match Decode.fromValue "$" decoder json with
              | Ok value -> value
              | Error er -> failwith er
          }

        member __.SerializeToBytes<'T>(o : 'T) : byte array =
            let t = o.GetType()
            let encoder = Encode.Auto.generateEncoderCached(t, ?isCamelCase=isCamelCase, ?extra=extra)
            use stream = new MemoryStream(DefaultBufferSize)
            use writer = new StreamWriter(stream, Utf8EncodingWithoutBom)
            use jsonWriter = new JsonTextWriter(writer)
            (encoder o).WriteTo(jsonWriter)
            jsonWriter.Flush()
            stream.ToArray()

        // TODO: Giraffe only calls this when writing chunked JSON (and setting Response header "Transfer-Encoding" to "chunked")
        // https://github.com/giraffe-fsharp/Giraffe/blob/f623527e1c537e77a07a5e594ced80f4f74016df/src/Giraffe/ResponseWriters.fs#L162
        // But it doesn't work. We need to prefix the chunk with the byte lenght, and finish it with \r\n
        // https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Transfer-Encoding#Directives
        member __.SerializeToStreamAsync (o : 'T) (stream : Stream) =
            upcast task {
                use streamWriter = new System.IO.StreamWriter(stream, Utf8EncodingWithoutBom, DefaultBufferSize, true)
                use jsonWriter = new JsonTextWriter(streamWriter)
                let encoder = Encode.Auto.generateEncoderCached<'T>(?isCamelCase=isCamelCase, ?extra=extra)
                do! (encoder o).WriteToAsync(jsonWriter)
            }
