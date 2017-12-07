[<RequireQualifiedAccess>]
module Route

#if DEBUG
let [<Literal>] Host = "/"
#else
let [<Literal>] Host = "https://mangelmaxime.github.io/Thot/"
#endif

let [<Literal>] Index = Host + "index.html"

[<RequireQualifiedAccess>]
module Json =

    let [<Literal>] Encode = Host + "json/" + "encode.html"
    let [<Literal>] Decode = Host + "json/" + "decode.html"
