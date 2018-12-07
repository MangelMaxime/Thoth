namespace Thoth.Json.Net

type Value = Newtonsoft.Json.Linq.JToken

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

[<AbstractClass>]
type BoxedDecoder() =
    abstract Decode: path : string * token: Value -> Result<obj, DecoderError>
    member this.BoxedDecoder: Decoder<obj> =
        fun path token -> this.Decode(path, token)

[<AbstractClass>]
type BoxedEncoder() =
    abstract Encode: value:obj -> Value
    member this.BoxedEncoder: Encoder<obj> = this.Encode

type ExtraCoders = Map<string, BoxedEncoder * BoxedDecoder>
