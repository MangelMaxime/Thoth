[<RequireQualifiedAccess>]
module Thot.Http

open Fable.Import
open Fable.Core.JsInterop
open System
open System.Collections.Generic
open Thot.Json

module private Helpers =

    open Fable.Core

    [<Emit("($0 !== undefined)")>]
    let isDefined (_: obj) : bool = jsNative

type Method =
    | Options
    | Get
    | Head
    | Post
    | Put
    | Delete
    | Trace
    | Patch
    | Connect

let private methodToString method =
    match method with
    | Options -> "OPTIONS"
    | Get -> "GET"
    | Head -> "HEAD"
    | Post -> "POST"
    | Put -> "PUT"
    | Delete -> "DELETE"
    | Trace -> "TRACE"
    | Patch -> "PATCH"
    | Connect -> "CONNECT"

type Header = string * string

type ResponseStatus =
    { Code : int
      Message : string }

type Expect<'Out> =
    { ResponseType : string
      ResponseToResult : Response -> Result<'Out, string> }

/// The response from a Request
and Response =
    { Url : string
      Status : ResponseStatus
      Headers : Dictionary<string, string>
      Body : string }

type Body =
    | EmptyBody
    | FormDataBody of Browser.FormData
    | StringBody of string * string
    | JsonBody of Encode.Value

/// Describe an HTTP request
type Request<'Out> =
    { /// Methid use to reach the server
      Method : Method
      /// List of the headers to send
      Headers : Header list
      /// Url of the server to reach
      Url : string
      /// Body content
      Body : Body
      /// Function used to describe the result we expect
      Expect : Expect<'Out>
      /// Optional timeout value
      Timeout : float option
      /// If set to true, then the cookies are send with the request
      WithCredentials : bool
      /// If set to a value, then the request will be send with a Time based query param (key of the param is the given string)
      CacheBuster : string option
      /// Query params to add to the request
      QueryParams : (string * string) list }

type Error =
    /// The url you provided isn't valid
    | BadUrl of string
    /// The request took too long to execute.
    | Timeout
    /// We couldn't reach the server. Expect, wifi was turn off, the server was down, etc.
    | NetworkError
    /// You got a response but the response code indictact an error.
    | BadStatus of Response
    /// You got a response with a good response code. However, the body of the response was something unexpected.
    /// Example: The `decoder` failed.
    | BadPayload of string * Response


///**Description**
///
/// Late you capture the whole response and apply you own `expect` function.
/// For example, we call `expectStringResponse` for `expectJson`.
///
///**Parameters**
///  * `responseToResult` - parameter of type `Response -> Result<'Out,string>`
///
///**Output Type**
///  * `Expect<'Out>`
///
let expectStringResponse (responseToResult : Response -> Result<'Out, string>) =
    { ResponseType = "text"
      ResponseToResult = responseToResult }


///**Description**
///
/// Expect the response to be a `string`.
///
///**Parameters**
///
///
///**Output Type**
///  * `Expect<string>`
///
let expectString : Expect<string> =
    expectStringResponse
        (fun response ->
            Ok response.Body
        )


///**Description**
///
/// Expect the response body to be a JSON value.
/// If the `decoder` fail, we provide the result error message, via `BadPayload` error.
///
///**Parameters**
///  * `decoder` - parameter of type `Decode.Decoder<'Out>`
///
///**Output Type**
///  * `Expect<'Out>`
///
let expectJson (decoder : Decode.Decoder<'Out>) : Expect<'Out> =
    expectStringResponse
        (fun response ->
            Decode.decodeString decoder response.Body
        )

let private parseHeaders (rawHeaders : string) =
    if not (Helpers.isDefined rawHeaders) then
        dict []
        |> Dictionary
    else
        let splitKeyVal (str : string) =
            let keyValue = str.Split([| ": " |], StringSplitOptions.None)

            match keyValue with
            | [| key; value |] -> Some (key, value)
            | _ -> None // Discard this malformed header

        rawHeaders.Split([| "\r\n" |], StringSplitOptions.None)
        |> Array.map splitKeyVal
        |> Array.filter Option.isSome
        |> Array.map (fun x -> x.Value)
        |> dict
        |> Dictionary

let private toResponse (xhr : Browser.XMLHttpRequest) : Response =
    { Status =
        { Code = int xhr.status
          Message = xhr.statusText }
      Headers = parseHeaders(xhr.getAllResponseHeaders())
      Url = !!xhr?responseURL
      Body = unbox<string> xhr.response }

let private handleResponse (xhr : Browser.XMLHttpRequest) (responseToResult : Expect<'Out>) : Result<'Out, Error> =
    let response : Response = toResponse xhr

    if response.Status.Code < 200 || response.Status.Code >= 300 then
        // Here we know that response if of type Response<string>
        // Because it's coming directly from the xhr answer, we didn't apply any transformation to it yet
        response |> BadStatus |> Error
    else
        match responseToResult.ResponseToResult response with
        | Ok result ->
            Ok result
        | Error msg ->
            BadPayload (msg, response) |> Error

let private configureRequest (xhr : Browser.XMLHttpRequest) (request : Request<'Out>) =
    request.Headers
    |> List.iter (fun (key, value) ->
        xhr.setRequestHeader(key, value)
    )

    xhr.responseType <- request.Expect.ResponseType
    xhr.withCredentials <- request.WithCredentials

    // Set timeout if configured
    match request.Timeout with
    | Some timeout ->
        xhr.timeout <- timeout
    | None -> ()

let private send (xhr : Browser.XMLHttpRequest) (request : Request<'Out>) =
    match request.Body with
    | EmptyBody ->
        xhr.send(box null)

    | StringBody (contentType, str) ->
        xhr.setRequestHeader("Content-Type", contentType)
        xhr.send(str)

    | FormDataBody data ->
        xhr.setRequestHeader("Content-Type", "multipart/form-data")
        xhr.send(data)

    | JsonBody value ->
        xhr.setRequestHeader("Content-Type", "application/json")
        xhr.send(Encode.encode 0 value)

let private replace (oldValue : string) (newValue : string) =
    (fun (str : string) -> str.Replace(oldValue, newValue) )

let private queryEscape =
    JS.encodeURIComponent >> replace "%20" "+"

let private queryPair ( key, value ) =
    queryEscape key + "=" + queryEscape value


let private joinUrlEncoded (args : (string * string) list) =
    String.Join("&", (List.map queryPair args))

let private requestWithMethodAndUrl method url =
    { Method = method
      Headers = []
      Url = url
      Body = EmptyBody
      Expect = expectStringResponse (fun _ -> Ok ())
      Timeout = None
      WithCredentials = false
      CacheBuster = None
      QueryParams = [] }


///**Description**
///
/// Start building a `GET` request with a given URL
///
///**Parameters**
///  * `url` - parameter of type `string`
///
///**Output Type**
///  * `Request<unit>`
///
let get (url : string) =
    requestWithMethodAndUrl Method.Get url


///**Description**
///
/// Start building a `POST` request with a given URL
///
///**Parameters**
///  * `url` - parameter of type `string`
///
///**Output Type**
///  * `Request<unit>`
///
let post (url : string) =
    requestWithMethodAndUrl Method.Post url


///**Description**
///
/// Start building a `OPTIONS` request with a given URL
///
///**Parameters**
///  * `url` - parameter of type `string`
///
///**Output Type**
///  * `Request<unit>`
///
let options (url : string) =
    requestWithMethodAndUrl Method.Options url


///**Description**
///
/// Start building a `HEAD` request with a given URL
///
///**Parameters**
///  * `url` - parameter of type `string`
///
///**Output Type**
///  * `Request<unit>`
///
let head (url : string) =
    requestWithMethodAndUrl Method.Head url


///**Description**
///
/// Start building a `PUT` request with a given URL
///
///**Parameters**
///  * `url` - parameter of type `string`
///
///**Output Type**
///  * `Request<unit>`
///
let put (url : string) =
    requestWithMethodAndUrl Method.Put url


///**Description**
///
/// Start building a `DELETE` request with a given URL
///
///**Parameters**
///  * `url` - parameter of type `string`
///
///**Output Type**
///  * `Request<unit>`
///
let delete (url : string) =
    requestWithMethodAndUrl Method.Delete url


///**Description**
///
/// Start building a `TRACE` request with a given URL
///
///**Parameters**
///  * `url` - parameter of type `string`
///
///**Output Type**
///  * `Request<unit>`
///
let trace (url : string) =
    requestWithMethodAndUrl Method.Trace url


///**Description**
///
/// Start building a `PATCH` request with a given URL
///
///**Parameters**
///  * `url` - parameter of type `string`
///
///**Output Type**
///  * `Request<unit>`
///
let patch (url : string) =
    requestWithMethodAndUrl Method.Patch url


///**Description**
///
/// Start building a `CONNECT` request with a given URL
///
///**Parameters**
///  * `url` - parameter of type `string`
///
///**Output Type**
///  * `Request<unit>`
///
let connect (url : string) =
    requestWithMethodAndUrl Method.Connect url

///**Description**
///
/// Add a header to a request
///
///**Parameters**
///  * `key` - parameter of type `string`
///  * `value` - parameter of type `string`
///  * `request` - parameter of type `Request<'a>`
///
///**Output Type**
///  * `Request<'a>`
///
let withHeader key value request : Request<_> =
    { request with Headers = (key, value) :: request.Headers }


///**Description**
///
/// Add severals header to a request
///
///**Parameters**
///  * `headers` - parameter of type `Header list`
///  * `request` - parameter of type `Request<'a>`
///
///**Output Type**
///  * `Request<'a>`
///
let withHeaders (headers : Header list) request : Request<_> =
    { request with Headers = headers @ request.Headers }


let private withBody body request : Request<_> =
    { request with Body = body }


///**Description**
///
/// Add a string body to a request
///
///**Parameters**
///  * `contentType` - parameter of type `string` - Example: `"application/json"`
///  * `value` - parameter of type `string`
///
///**Output Type**
///  * `Request<'a> -> Request<'a>`
///
let withStringBody (contentType : string) (value : string) =
    withBody <| StringBody (contentType, value)


///**Description**
///
/// Add a Json body to a request
///
///**Parameters**
///  * `value` - parameter of type `Encode.Value`
///
///**Output Type**
///  * `Request<'a> -> Request<'a>`
///
let withJsonBody (value : Encode.Value) =
    withBody <| JsonBody value

let private toMultipart (keyValues : (string * string) list) =
    let formData = Browser.FormData.Create()

    keyValues
    |> List.iter (fun (key, value) ->
        formData.append(key, value)
    )

    FormDataBody formData


///**Description**
///
/// Add a FormData string body to a request
///
///**Parameters**
///  * `keyValues` - parameter of type `(string * string) list`
///
///**Output Type**
///  * `Request<'a> -> Request<'a>`
///
let withMultipartStringBody (keyValues : (string * string) list) =
    withBody <| toMultipart keyValues

///**Description**
///
/// Set the `timeout` setting on the request
///
///**Parameters**
///  * `timeout` - parameter of type `float`
///  * `request` - parameter of type `Request<'a>`
///
///**Output Type**
///  * `Request<'a>`
///
let withTimeout timeout (request : Request<_>) : Request<_> =
    { request with Timeout = Some timeout }


///**Description**
///
/// Set the `WithCredentials` flags on the request to true
///
///**Parameters**
///  * `request` - parameter of type `Request<'a>`
///
///**Output Type**
///  * `Request<'a>`
///
let withCredentials (request : Request<_>) : Request<_> =
    { request with WithCredentials = true }


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
    { Method = request.Method
      Headers = request.Headers
      Url = request.Url
      Body = request.Body
      Expect = expect
      Timeout = request.Timeout
      WithCredentials = request.WithCredentials
      CacheBuster = request.CacheBuster
      QueryParams = request.QueryParams }


///**Description**
///
/// Add query params to URL of the request
///
///**Parameters**
///  * `queryParams` - parameter of type `(string * string) list`
///  * `request` - parameter of type `Request<'a>`
///
///**Output Type**
///  * `Request<'a>`
///
let withQueryParams (queryParams : (string * string) list) (request : Request<_>) : Request<_> =
    { request with QueryParams = request.QueryParams @ queryParams }


///**Description**
///
/// Send the request with a Time based cache buster added to the URL.
/// You provide the key for the extra query param.
///
///**Parameters**
///  * `paramName` - parameter of type `string`
///  * `request` - parameter of type `Request<'a>`
///
///**Output Type**
///  * `Request<'a>`
///
let withCacheBuster (paramName : string) (request : Request<_>) : Request<_> =
    { request with CacheBuster = Some paramName }


///**Description**
///
/// Execute the given request and pass the result in the provided `dispatch`
///
///**Parameters**
///  * `request` - parameter of type `Request<'Out>`
///  * `dispatch` - parameter of type `Result<'Out,HttpError> -> unit`
///
///**Output Type**
///  * `unit`
let toTask (request : Request<'Out>) dispatch =
    let xhr = Browser.XMLHttpRequest.Create()

    // Add CacheBuster query param if needed
    let calcRequest =
        match request.CacheBuster with
        | Some paramName ->
            request
            |> withQueryParams [ (paramName, string DateTime.Now.Ticks) ]
        | None -> request

    let encodedParams =
        joinUrlEncoded calcRequest.QueryParams

    let fullUrl =
        if String.IsNullOrEmpty encodedParams then
            calcRequest.Url
        else
            calcRequest.Url + "?" + encodedParams

    xhr.addEventListener_error(fun _ ->
        NetworkError |> Error |> dispatch
    )

    xhr.addEventListener_timeout(fun _ ->
        Timeout |> Error |> dispatch
    )

    xhr.addEventListener_load(fun _ ->
        handleResponse xhr calcRequest.Expect
        |> dispatch
    )

    try
        xhr.``open``(methodToString calcRequest.Method, fullUrl)
        configureRequest xhr calcRequest
        send xhr calcRequest

    with
        | _ -> BadUrl calcRequest.Url |> Error |> dispatch
