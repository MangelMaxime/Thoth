[<RequireQualifiedAccess>]
module Decode

open Renderer
open Docs.Helpers

open Fulma.Elements
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Core

[<Pojo>]
type DangerousInnerHtml =
    { __html : string }

let htmlFromMarkdown str =
    div [ DangerouslySetInnerHTML { __html = makeHtml str } ] [ ]

let private content =
    """
# Decode

Turn Json value into F# values.

By using a Decoder instead of Fable `ofJson` function, you will garantee the JSON structure is correct.
This is especially useful if you use Fable without sharing your domain with the server.

*This mobule is inspired by [Json.Decode from Elm](http://package.elm-lang.org/packages/elm-lang/core/latest/Json-Decode)
and [elm-decode-pipeline](http://package.elm-lang.org/packages/NoRedInk/elm-decode-pipeline/latest).*

## What is a Decoder ?

Here is the signature of a `Decoder`:

```fsharp
type Decoder<'T> = obj -> Result<'T, DecoderError>
```

This is taking an "untyped" value, and check if it have the expected structure. If the structure is correct,
then you get an `Ok` result otherwise an `Error` explaining why the decoder failed.

## Primitives decoders

There is 4 primitives decoders:

- `string : Decoder<String>`
- `int : Decoder<Int>`
- `float : Decoder<Float>`
- `bool : Decoder<Bool>`

```fsharp
open Thot.Json.Decode

> decodeString string "\"maxime\""
Ok "maxime"

> decodeString int "25"
Ok 25

> decodeString bool "true"
Ok "maxime"

> decodeString float "true"
Err "Expecting a float but instead got: true"
```

With this 4 primitives decoders we can handle the basic Json values.

## Data Structure

Data structure are really common so they have special decoders.

- `list : Decoder<'value> -> Decoder<'value list>`
- `array : Decoder<'value> -> Decoder<'value array>`
    """.Trim()

let body =
    Content.content [ ]
        [ br [ ]
          htmlFromMarkdown content ]

let render () =
    Page.render {
        Page = Route.Decode
        Title = None
        Body = body |> parseReactStatic
    }
