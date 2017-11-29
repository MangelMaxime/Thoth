namespace Renderer

[<RequireQualifiedAccess>]
module Navbar =

    open Fulma.Components
    open Fable.Helpers.React
    open Fable.Helpers.React.Props
    open Fulma.Layouts
    open Fulma.Elements
    open Fulma.Elements.Form
    open Fulma.Extra.FontAwesome

    type ActivePage =
        | Decode
        | Encode
        | Index

    let private shadow =
        div [ ClassName "bd-special-shadow"
              Style [ Opacity 1.
                      Transform "scaleY(1)" ] ]
            [ ]

    let private viewSimpleIcon =
        let inline itemKey key = Navbar.Item.props [ Key ("simple-icon-"+ key) ]

        [ Navbar.item_a [ itemKey "Github" ]
            [ Icon.faIcon [ ]
                [ Fa.icon Fa.I.Github
                  Fa.faLg ] ]
          Navbar.item_a [ itemKey "Twitter" ]
            [ Icon.faIcon [ ]
                [ Fa.icon Fa.I.Twitter
                  Fa.faLg ] ]
        ] |> ofList

    let private viewButton =
        Navbar.item_div [ ]
            [ Field.field_div [ Field.isGrouped ]
                [ Control.control_p [ ]
                    [ Button.button_a [  ]
                        [ Icon.faIcon [ ]
                            [ Fa.icon Fa.I.Twitter
                              Fa.faLg ]
                          span [ ] [ str "Tweet" ]
                        ]
                    ]
                  Control.control_p [ ]
                    [ Button.button_a [ ]
                        [ Icon.faIcon [ ]
                            [ Fa.icon Fa.I.Github
                              Fa.faLg ]
                          span [ ] [ str "Github" ]
                        ]
                    ]
                ]
            ]

    let private navbarEnd =
        Navbar.end_div [ ]
            [ viewSimpleIcon
              viewButton ]

    let private navbarMenu activeMenu =
        Navbar.menu [ ]
            [ Navbar.item_a [ if activeMenu = Decode then yield Navbar.Item.isActive ]
                [ str "Decode" ]
              Navbar.item_a [ if activeMenu = Encode then yield Navbar.Item.isActive ]
                [ str "Encode" ] ]

    let private navbarBrand =
        Navbar.brand_div [ ]
            [ Navbar.item_a [ Navbar.Item.props [ Href "https://mangelmaxime.github.io/Thot" ] ]
                [ Heading.p [ Heading.is4 ]
                    [ str "Thot" ] ] ]

    let render activeMenu =
        Navbar.navbar [ Navbar.isPrimary
                        Navbar.customClass "is-fixed-top" ]
            [ shadow
              Container.container [ ]
                [ navbarBrand
                  navbarMenu activeMenu
                  navbarEnd ] ]
        |> Docs.Helpers.parseReactStatic
