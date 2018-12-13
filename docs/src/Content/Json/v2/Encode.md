<article class="message is-info">
  <div class="message-body">
<strong>Version 2</strong> of Thoth.Json and Thoth.Json.Net <strong>only support</strong> Fable 2.
  </div>
</article>

# Encode

Module for turning F# values into JSON values.

*This module is inspired by [Json.Encode from Elm](http://package.elm-lang.org/packages/elm-lang/core/latest/Json-Encode).*

## How to use it ?

```fsharp
    open Thoth.Json

    let person =
        Encode.object
            [ "firstname", Encode.string "maxime"
              "surname", Encode.string "mangel"
              "age", Encode.int 25
              "address", Encode.object
                            [ "street", Encode.string "main street"
                              "city", Encode.string "Bordeaux" ]
            ]

    let compact = Encode.toString 0 person
    // {"firstname":"maxime","surname":"mangel","age":25,"address":{"street":"main street","city":"Bordeaux"}}

    let readable = Encode.toString 4 person
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

## Auto encoder

```fsharp
type User =
    { Id : int
      Name : string
      Email : string
      Followers : int }

let user =
    { Id = 0
      Name = "maxime"
      Email = "mail@domain.com"
      Followers = 0 }

let json = Encode.Auto.toString(4, user)
// {
//     "Id": 0,
//     "Name": "maxime",
//     "Email": "mail@domain.com",
//     "Followers": 0
// }
```

Please see "Auto decoder" for more information about generating coders automatically.
