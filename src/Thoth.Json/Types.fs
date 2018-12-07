namespace Thoth.Json

type Value = obj

type ErrorReason =
    | BadPrimitive of string * Value
    | BadPrimitiveExtra of string * Value * string
    | BadType of string * Value
    | BadField of string * Value
    | BadPath of string * Value * string
    | TooSmallArray of string * Value
    | FailMessage of string
    | BadOneOf of string list

type DecoderError = string * ErrorReason

type Decoder<'T> = string -> Value -> Result<'T, DecoderError>

type Encoder<'T> = 'T -> Value

type BoxedDecoder = Decoder<obj>

type BoxedEncoder = Encoder<obj>

type ExtraCoders = Map<string, BoxedEncoder * BoxedDecoder>

module internal Cache =
    open System.Collections.Generic

    type Cache<'Value>() =
        let cache = Dictionary<string, 'Value>()
        member __.GetOrAdd(key, factory) =
            match cache.TryGetValue(key) with
            | true, x -> x
            | false, _ ->
                let x = factory()
                cache.Add(key, x)
                x

    // Tree shaking will remove this if not used
    // so no need to make them lazy in Fable
    let Encoders = Cache<BoxedEncoder>()
    let Decoders = Cache<BoxedDecoder>()
