module Tests.Http

open Thot
open Thot.Json
open Fable.Core
open Fable.Core.Testing
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Node

[<Global>]
let it (msg: string) (f: (obj->unit)->unit): unit = jsNative

[<Global>]
let describe (msg: string) (f: unit->unit): unit = jsNative

[<Global>]
let before (f: unit->unit): unit = jsNative

[<Global>]
let after (f: unit->unit): unit = jsNative

[<Emit("global.XMLHttpRequest = require(\"xhr2\");")>]
let mockXMLHttpRequest _ = ()
mockXMLHttpRequest ()

type IExports =
    abstract create : unit -> express.Express
    abstract router : string -> express.RequestHandler
    abstract defaults : unit -> express.RequestHandler

[<Import("*", "json-server")>]
let jsonServer : IExports = jsNative

let mutable serverInstance = Unchecked.defaultof<Http.Server>

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

describe "Thot.Http" <| fun _ ->

    before <| fun _ ->
        let dbFile = Exports.path.join(Globals.__dirname, "db.json")
        try
            Exports.fs.unlinkSync(!^dbFile)
        with
            | _ -> ()
        Exports.fs.writeFileSync(dbFile,
            """
{
  "posts": [
    { "id": 1, "title": "json-server", "author": "typicode" }
  ]
}
            """
        )

        let server = jsonServer.create()
        let router = jsonServer.router(dbFile)
        let middlewares = jsonServer.defaults()

        server.``use``(middlewares) |> ignore
        server.``use``(router) |> ignore
        serverInstance <- server.listen(3000, !!ignore)

    after <| fun _ ->
        serverInstance.close() |> ignore

    it "`getString` works" <| fun d ->
        let expected =
            """
{
  "id": 1,
  "title": "json-server",
  "author": "typicode"
}
            """.Trim()

        let request =
            Http.get "http://localhost:3000/posts/1"
            |> Http.withExpect Http.expectString

        Http.toTask
            request
            (fun response ->
                match response with
                | Ok success ->
                    try
                        Assert.AreEqual(expected, success)
                        d()
                    with
                        | ex -> d ex
                | Error error ->
                    d error
            )

    it "`get` works" <| fun d ->
        let expected : Post =
            { Id = 1
              Title = "json-server"
              Author = "typicode" }

        let request =
            Http.get "http://localhost:3000/posts/1"
            |> Http.withExpect (Http.expectJson Post.Decoder)

        Http.toTask
            request
            (fun response ->
                match response with
                | Ok success ->
                    try
                        Assert.AreEqual(expected, success)
                        d()
                    with
                        | ex -> d ex
                | Error error ->
                    d error
            )

    it "`post` works" <| fun d ->
        let expected =
            { Id = 2
              Title = "Thot"
              Author = "Mangel Maxime" }

        let body =
            Encode.object
                [ "title", Encode.string "Thot"
                  "author", Encode.string "Mangel Maxime" ]

        let request =
            Http.post "http://localhost:3000/posts"
            |> Http.withJsonBody body
            |> Http.withExpect (Http.expectJson Post.Decoder)

        Http.toTask
            request
            (function
                | Ok response ->
                    try
                        Assert.AreEqual(expected, response)
                        d()
                    with
                        | ex -> d ex
                | Error error ->
                    d error
            )

    it "`request` works" <| fun d ->
        let expected = ()

        let request =
            Http.delete "http://localhost:3000/posts/2"

        Http.toTask
            request
            (function
                | Ok response ->
                    try
                        Assert.AreEqual(expected, response)
                        d()
                    with
                        | ex -> d ex
                | Error error ->
                    d error
            )

    it "Capture `BadStatus` capture errors works" <| fun d ->
        let expected : Http.Response =
            { Url = "http://localhost:3000/posts/2"
              Status = { Code = 404
                         Message = "Not Found" }
              Headers = System.Collections.Generic.Dictionary<string, string>()
              Body = "{}" }

        let request =
            Http.get "http://localhost:3000/posts/2"
            |> Http.withExpect (Http.expectJson Post.Decoder)

        Http.toTask
            request
            (function
                | Ok _ ->
                    d "Should not have a valid response"
                | Error error ->
                    match error with
                    | Http.HttpError.BadStatus response ->
                        // We ignore the headers check in the test because it's dynamic.
                        // For example, their is a date in it etc.
                        Assert.AreEqual(expected.Url, response.Url)
                        Assert.AreEqual(expected.Status, response.Status)
                        Assert.AreEqual(expected.Body, response.Body)
                        d()
                    | x -> d ("Bad Http.Error type: " + string x)
            )

    it "Capture `NetworkError` errors works" <| fun d ->
        let request =
            Http.get "http://non-existant-server.com/"
            |> Http.withExpect (Http.expectJson Post.Decoder)

        Http.toTask
            request
            (function
                | Ok _ ->
                    d "Should not have a valid response"
                | Error error ->
                    match error with
                    | Http.HttpError.NetworkError ->
                        d()
                    | x -> d ("Bad Http.Error type: " + string x)
            )

    it "Capture `Timeout` errors works" <| fun d ->
        let request =
            Http.get "http://localhost:3000/posts/1"
            |> Http.withTimeout 1.

        Http.toTask
            request
            (function
                | Ok _ ->
                    d "Should not have a valid response"
                | Error error ->
                    match error with
                    | Http.HttpError.Timeout ->
                        d()
                    | x -> d ("Bad Http.Error type: " + string x)
            )

    it "Capture `BadUrl` errors works" <| fun d ->
        let request =
            Http.get "invalid-url"

        Http.toTask
            request
            (function
                | Ok _ ->
                    d "Should not have a valid response"
                | Error error ->
                    match error with
                    | Http.HttpError.BadUrl invalidUrl ->
                        Assert.AreEqual("invalid-url", invalidUrl)
                        d()
                    | x -> d ("Bad Http.Error type: " + string x)
            )

    it "Capture `BadPayload` errors works" <| fun d ->
        let expectedResponse : Http.Response =
            { Url = "http://localhost:3000/posts/1"
              Status = { Code = 200
                         Message = "OK" }
              Headers = System.Collections.Generic.Dictionary<string, string>()
              Body = "{\n  \"id\": 1,\n  \"title\": \"json-server\",\n  \"author\": \"typicode\"\n}" }

        let expectedMsg = "Expecting a string but instead got: {\n    \"id\": 1,\n    \"title\": \"json-server\",\n    \"author\": \"typicode\"\n}"

        let request =
            Http.get "http://localhost:3000/posts/1"
            |> Http.withExpect (Http.expectJson Decode.string)

        Http.toTask
            request
            (function
                | Ok _ ->
                    d "Should not have a valid response"
                | Error error ->
                    match error with
                    | Http.HttpError.BadPayload (msg, response) ->
                        // We ignore the headers check in the test because it's dynamic.
                        // For example, their is a date in it etc.
                        Assert.AreEqual(expectedMsg, msg)
                        Assert.AreEqual(expectedResponse.Url, response.Url)
                        Assert.AreEqual(expectedResponse.Status, response.Status)
                        Assert.AreEqual(expectedResponse.Body, response.Body)
                        d()
                    | x -> d ("Bad Http.Error type: " + string x)
            )
