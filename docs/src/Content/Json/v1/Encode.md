# Encode

Module for turning F# values into JSON values.

*This module is inspired by [Json.Encode from Elm](http://package.elm-lang.org/packages/elm-lang/core/latest/Json-Encode).*

## How to use it ?

```fsharp
    open Thoth.Json.Encode

    let person =
        object
            [ "firstname", string "maxime"
              "surname", string "mangel"
              "age", int 25
              "address", object
                            [ "street", string "main street"
                              "city", string "Bordeaux" ]
            ]

    let compact = encode 0 person
    // {"firstname":"maxime","surname":"mangel","age":25,"address":{"street":"main street","city":"Bordeaux"}}

    let readable = encode 4 person
    // {
    //     "firstname": "maxime",
    //     "surname": "mangel",
    //     "age": 25,
    //     "address": {
    //         "street": "main street",
    //         "city": "Bordeaux"
    //     }
    // }
```
