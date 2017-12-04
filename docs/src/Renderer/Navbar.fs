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

    let private shadow =
        div [ ClassName "bd-special-shadow"
              Style [ Opacity 1.
                      Transform "scaleY(1)" ] ]
            [ ]

    let private viewSimpleIcon =
        let inline props key url = Navbar.Item.props [ Key ("simple-icon-"+ key)
                                                       Href url ]

        [ Navbar.item_a [ props "Github" "https://github.com/MangelMaxime/thot"]
            [ Icon.faIcon [ ]
                [ Fa.icon Fa.I.Github
                  Fa.faLg ] ]
          Navbar.item_a [ props "Twitter" "https://twitter.com/FableCompiler" ]
            [ Icon.faIcon [ ]
                [ Fa.icon Fa.I.Twitter
                  Fa.faLg ] ]
        ] |> ofList

    let private viewButton =
        Navbar.item_div [ ]
            [ Field.field_div [ Field.isGrouped ]
                [ Control.control_p [ ]
                    [ Button.button_a [ Button.href "https://twitter.com/FableCompiler" ]
                        [ Icon.faIcon [ ]
                            [ Fa.icon Fa.I.Twitter
                              Fa.faLg ]
                          span [ ] [ str "Tweet" ]
                        ]
                    ]
                  Control.control_p [ ]
                    [ Button.button_a [ Button.href "https://github.com/MangelMaxime/thot" ]
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

    let private navbarJson activeMenu =
        let jsonPage =
            match activeMenu with
            | Route.Json jsonPage -> jsonPage
            | Route.Index -> Unchecked.defaultof<Route.JsonPage>

        Navbar.dropdown_div [ ]
            [ Navbar.item_a [ if jsonPage = Route.Decode then yield Navbar.Item.isActive
                              yield Navbar.Item.props [ Href (Route.toUrl (Route.Json Route.Decode)) ] ]
                [ str "Decode" ]
              Navbar.item_a [ if jsonPage = Route.Encode then yield Navbar.Item.isActive
                              yield Navbar.Item.props [ Href (Route.toUrl (Route.Json Route.Encode)) ] ]
                [ str "Encode" ] ]

    let private navbarMenu activeMenu =
        let jsonActive =
            match activeMenu with
            | Route.Json jsonPage -> true
            | Route.Index -> false

        Navbar.menu [ ]
            [ Navbar.item_div [ Navbar.Item.hasDropdown
                                Navbar.Item.isHoverable ]
                [ Navbar.link_div [ ]
                    [ str "Json" ]
                  navbarJson activeMenu ] ]

    let private navbarBrand =
        Navbar.brand_div [ ]
            [ Navbar.item_a [ Navbar.Item.props [ Href (Route.toUrl Route.Index) ] ]
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
