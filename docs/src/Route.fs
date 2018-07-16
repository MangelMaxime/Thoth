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

    module v1 =
        let [<Literal>] Encode = Host + "json/" + "v1/" + "encode.html"
        let [<Literal>] Decode = Host + "json/" + "v1/" + "decode.html"
        let [<Literal>] Net = Host + "json/" + "v1/" + "net.html"

    module v2 =
        let [<Literal>] Encode = Host + "json/" + "v2/" + "encode.html"
        let [<Literal>] Decode = Host + "json/" + "v2/" + "decode.html"
        let [<Literal>] Net = Host + "json/" + "v2/" + "net.html"

[<RequireQualifiedAccess>]
module Elmish =

    let [<Literal>] Debouncer = Host + "elmish/" + "debouncer.html"

    module Toast =
        let [<Literal>] Docs = Host + "elmish/" + "toast_docs.html"
        let [<Literal>] Demo = Host + "elmish/" + "toast_demo.html"
