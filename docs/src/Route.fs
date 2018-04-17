[<RequireQualifiedAccess>]
module Route

#if DEBUG
let [<Literal>] Host = "/"
#else
let [<Literal>] Host = "https://mangelmaxime.github.io/Thoth/"
#endif

let [<Literal>] Index = Host + "index.html"

[<RequireQualifiedAccess>]
module Json =

    let [<Literal>] Encode = Host + "json/" + "encode.html"
    let [<Literal>] Decode = Host + "json/" + "decode.html"
    let [<Literal>] Net = Host + "json/" + "net.html"

[<RequireQualifiedAccess>]
module Http =

    let [<Literal>] Basic = Host + "http/" + "basic.html"
    let [<Literal>] Elmish = Host + "http/" + "elmish.html"
