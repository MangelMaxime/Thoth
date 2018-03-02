# Http

Modules for building HTTP requests in a chainable way.

*This module is inspired by [Http from Elm](http://package.elm-lang.org/packages/elm-lang/http/latest)
and [elm-http-builder](http://package.elm-lang.org/packages/lukewestby/elm-http-builder/latest).*

## Usage example

The following code will build a request with:

- the url: `http://localhost:3000/posts?page=2`
- the method: `GET`
- a custom header: `x-my-custom-header: my-token`
- a timeout set to `20ms`
- the cookies include
- the expecting result is a `string` value

```fsharp
open Thot

let getPostByPage page : Request<string> =
    Http.get "http://localhost:3000/posts"
    |> Http.withQueryParams [ "page", string page ]
    |> Http.withHeader "x-my-custom-header" "my-token"
    |> Http.withTimeout 20.
    |> Http.withCredentials
    |> Http.withExpect Http.expectString

Http.toTask
    (getPostBypage 2)
    (fun (response : Result<string, Http.Error>) ->
        // Do something here
    )
```

## How to handle the anwser ?

When building a request with Thot.Http, you can describe what anwser you are **expecting**.

In order to describe the expecting result add `Http.withExpect` to your request chain.

Here is the documentation about it:

```fsharp
///**Description**
///
/// Set the `expect` value for the request
///
///**Parameters**
///  * `expect` - parameter of type `Expect<'Out>`
///  * `request` - parameter of type `Request<'a>`
///
///**Output Type**
///  * `Request<'Out>`
///
let withExpect (expect : Expect<'Out>) (request : Request<'a>) : Request<'Out> =
```

Thot.Http provide three ways to set the `expect` argument:

- `Http.expectString`, the request will return a string
- `Http.expectJson`, the request will return a Json value
- `Http.expectStringResponse`, the request will return the generic `Reponse` record

### Expect a string

```fsharp
open Thot

let getPostByPage page : Request<string> =
    Http.get "http://localhost:3000/posts/1"
    |> Http.withExpect Http.expectString

Http.toTask
    (getPostBypage 2)
    (fun (response : Result<string, Http.Error>) ->
        // For example:
        // Ok "{\"id\":1,\"title\":\"json-server\",\"author\":\"typicode\"}"
    )
```

### Expect a Json

When expecting a Json, you need to specify the [Decoder](https://mangelmaxime.github.io/Thot/json/decode.html) to apply on the server response.

If the [Decoder](https://mangelmaxime.github.io/Thot/json/decode.html) fail, then you will have a error of type `BadPayload` where the first argument, is the decoder message.

```fsharp
open Thot

type Post =
    { Id : int
      Title : string
      Author : string }

    static member Decoder =
        Decode.decode
            (fun id title author ->
                { Id = id
                  Title = title
                  Author = author } : Post)
            |> Decode.required "id" Decode.int
            |> Decode.required "title" Decode.string
            |> Decode.required "author" Decode.string

let getPostByPage page : Request<string> =
    Http.get "http://localhost:3000/posts/1"
    |> Http.withExpect (Http.expectJson Post.Decoder)

Http.toTask
    (getPostBypage 2)
    (fun (response : Result<Post, Http.Error>) ->
        // For example:
        // Ok { Id = 1
        //      Title = "json-server"
        //      Author = "typicode" }

    )
```

### Expect generic Response

When using `Http.expectStringResponse`, you will need to specify a function taking the generci response record and extract the data by yourself.

For example here is how we implement the `Http.expectJson` function.

```fsharp
// The Response type is given by Thot.Http
// We put it here just for reference
type Response =
    { Url : string
      Status : ResponseStatus
      Headers : Dictionary<string, string>
      Body : string }

let expectJson (decoder : Decode.Decoder<'Out>) : Expect<'Out> =
    expectStringResponse
        (fun (response : Response) ->
            Decode.decodeString decoder response.Body
        )
```
