namespace Thoth.Json.Giraffe

open System.IO
open Giraffe
open Giraffe.Serialization.Json
open Thoth.Json.Net

type ThothSerializer () =
    interface IJsonSerializer with
        member __.Serialize (o : obj) =
            Encode.Auto.toString(0, o)

        member __.Deserialize<'T> (json : string) =
            Decode.Auto.unsafeFromString<'T>(json)

        member __.Deserialize<'T> (stream : Stream) =
            use sr = new StreamReader(stream, true)
            let str = sr.ReadToEnd()
            Decode.Auto.unsafeFromString<'T>(str)

        member __.DeserializeAsync<'T> (stream : Stream) =
            task {
                use sr = new StreamReader(stream, true)
                let str = sr.ReadToEnd()
                return Decode.Auto.unsafeFromString<'T>(str)
            }
