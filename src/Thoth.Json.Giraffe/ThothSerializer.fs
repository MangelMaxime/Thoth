namespace Thoth.Json.Giraffe

open System.IO
open Giraffe.Serialization.Json
open Thoth.Json.Net
open System.Text
open System.Threading.Tasks

type ThothSerializer () =
    let Utf8EncodingWithoutBom = new UTF8Encoding(false)
    let DefaultBufferSize = 1024

    interface IJsonSerializer with
        member __.SerializeToString (o : 'T) =
            Encode.Auto.toString(0, o)

        member __.Deserialize<'T> (json : string) =
            Decode.Auto.unsafeFromString<'T>(json)

        member __.Deserialize<'T> (bytes : byte[]) =
            let json =  Encoding.UTF8.GetString bytes
            Decode.Auto.unsafeFromString<'T>(json)

        member __.DeserializeAsync<'T> (stream : Stream) =
            use sr = new StreamReader(stream, true)
            let str = sr.ReadToEnd()
            Task.FromResult(Decode.Auto.unsafeFromString<'T>(str))

        member __.SerializeToBytes<'T>(o : 'T) : byte array =
            Encode.Auto.toString(0, o)
            |> Encoding.UTF8.GetBytes

        member __.SerializeToStreamAsync (o : 'T) (stream : Stream) =
            use sw = new StreamWriter(stream, Utf8EncodingWithoutBom, DefaultBufferSize, true)
            let json = Encode.Auto.toString(0, o)
            sw.Write(json)
            Task.CompletedTask
