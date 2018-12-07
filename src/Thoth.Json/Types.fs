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
