# Decode

Turn JSON values into F# values.

By using a Decoder instead of Fable `ofJson` function, you will be guaranteed that the JSON structure is correct.
This is especially useful if you use Fable without sharing your domain with the server.

*This module is inspired by [Json.Decode from Elm](http://package.elm-lang.org/packages/elm-lang/core/latest/Json-Decode)
and [elm-decode-pipeline](http://package.elm-lang.org/packages/NoRedInk/elm-decode-pipeline/latest).*

As so, to complete this documentation, you can also take a look at the [Elm documentation](https://guide.elm-lang.org/interop/json.html).

## What is a Decoder ?

Here is the signature of a `Decoder`:

```fsharp
type Decoder<'T> = obj -> Result<'T, DecoderError>
```

This is taking an "untyped" value and checking if it has the expected structure. If the structure is correct,
then you get an `Ok` result, otherwise an `Error` explaining why the decoder failed.

## Primitives decoders

- `string : Decoder<String>`
- `int : Decoder<Int>`
- `float : Decoder<Float>`
- `bool : Decoder<Bool>`

```fsharp
open Thoth.Json.Decode

> decodeString string "\"maxime\""
val it : Result<string, string> = Ok "maxime"

> decodeString int "25"
val it : Result<int, string> = Ok 25

> decodeString bool "true"
val it : Result<bool, string> = Ok true

> decodeString float "true"
val it : Result<float, string> = Err "Expecting a float but instead got: true"
```

With these primitives decoders we can handle the basic JSON values.

## Collections

There are special decoders for the following collections.

- `list : Decoder<'value> -> Decoder<'value list>`
- `array : Decoder<'value> -> Decoder<'value array>`

```fsharp
open Thoth.Json.Decode

> decodeString (array int) "[1, 2, 3]"
val it : Result<int [], string> =  Ok [|1, 2, 3|]

> decodeString (list string) """["Maxime", "Alfonso", "Vesper"]"""
val it : Result<string list, string> = Ok ["Maxime", "Alfonso", "Vesper"]
```

## Decoding Objects

In order to decode objects, you can use:

- `field : string -> Decoder<'value> -> Decoder<'value>`
    - Decode a JSON object, requiring a particular field.
- `at : string list -> Decoder<'value> -> Decoder<'value>`
    - Decode a JSON object, requiring certain path.

```fsharp
open Thoth.Json.Decode

> decodeString (field "x" int) """{"x": 10, "y": 21}"""
val it : Result<int, string> = Ok 10

> decodeString (field "y" int) """{"x": 10, "y": 21}"""
val it : Result<int, string> = Ok 21
```

**Important:**

These two decoders only take into account the field or path. The object can have other fields/paths with other content.

### Map functions

To get data from several fields and convert them into a record you will need to use the `map` functions
like `map2`, `map3`, ..., `map8`.

```fsharp
open Thoth.Json.Decode

type Point =
    { X : int
      Y : int }

    static member Decoder : Decoder<Point> =
        map2 (fun x y ->
                { X = x
                  Y = y } : Point)
             (field "x" int)
             (field "y" int)

> decodeString Point.Decoder """{"x": 10, "y": 21}"""
val it : Result<Point, string> = Ok { X = 10; Y = 21 }
```

### Pipeline decode style

When working with a larger object or if you prefer to use the `(|>)` operator, you can use the pipeline helpers.

```fsharp
open Thoth.Json.Decode

type Point =
    { X : int
      Y : int }

    static member Decoder =
        decode
            (fun x y ->
                { X = x
                  Y = y } : Point)
            |> required "x" int
            |> required "y" int

> decodeString Point.Decoder """{"x": 10, "y": 21}"""
val it : Result<Point, string> = Ok { X = 10; Y = 21 }
```
