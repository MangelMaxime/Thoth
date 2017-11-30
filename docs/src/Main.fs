module Main

open Renderer
open Docs.Helpers


open Fulma.Layouts
open Fulma.Elements
open Fable.Helpers.React

let decodeApi =
    Container.container [ ]
        [ br [ ]
          Heading.h1 [ ]
            [ str "Decode" ]
          Api.render (resolve "${entryDir}/../src/Json/Encode.fs") ]

Page.render {
    ActivePage = Navbar.Decode
    Title = None
    Body = decodeApi
}
