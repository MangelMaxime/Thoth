[<RequireQualifiedAccess>]
module Route

type Page =
    | Decode
    | Encode
    | Index

#if DEBUG
let host = ""
#else
let host = "https://mangelmaxime.github.io/Thot"
#endif

let toUrl url =
    match url with
    | Decode -> "decode"
    | Encode -> "encode"
    | Index -> "index"
    |> fun url -> host + "/" + url + ".html"
