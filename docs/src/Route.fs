[<RequireQualifiedAccess>]
module Route

type JsonPage =
    | Decode
    | Encode

type Page =
    | Json of JsonPage
    | Index

#if DEBUG
let host = ""
#else
let host = "https://mangelmaxime.github.io/Thot"
#endif

let toUrl url =
    match url with
    | Json subPage ->
        match subPage with
        | Decode -> "json/decode"
        | Encode -> "json/encode"
    | Index -> "index"
    |> fun url -> host + "/" + url + ".html"
