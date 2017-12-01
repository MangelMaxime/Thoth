[<RequireQualifiedAccess>]
module Route

type ActivePage =
    | Decode
    | Encode
    | Index

let toUrl url =
    match url with
    | Decode -> "decode"
    | Encode -> "encode"
    | Index -> "index"
    |> fun url -> url + ".html"
