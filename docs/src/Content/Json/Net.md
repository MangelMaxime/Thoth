# .Net & NetCore support

You can share your decoders and encoders **between your client and server**.

In order to use Thot.Json API on .Net or NetCore you need to use `Thot.Json.Net` package.

## Code sample

```fsharp
// By adding this condition, you can share you code between your client and server
#if FABLE_COMPILER
open Thot.Json
#else
open Thot.Json.Net
#endif

type User =
    { Id : int
      Name : string
      Email : string
      Followers : int }

    static member Decoder =
        Decode.decode
            (fun id email name followers ->
                { Id = id
                  Name = name
                  Email = email
                  Followers = followers })
            |> Decode.required "id" Decode.int
            |> Decode.required "email" Decode.string
            |> Decode.optional "name" Decode.string ""
            |> Decode.hardcoded 0

    static member Encoder (user : User) =
        Encode.object
            [ "id", Encode.int user.Id
              "name", Encode.string user.Name
              "email", Encode.string user.Email
              "followers", Encode.int user.Followers
            ]
```
