[<RequireQualifiedAccess>]
module Components.Navbar

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
        let inline className cls = Navbar.Item.customClass cls

        [ Navbar.item_a [ props "Github" "https://github.com/MangelMaxime/thot" ]
            [ Icon.faIcon [ ]
                [ Fa.icon Fa.I.Github
                  Fa.faLg ] ]
          Navbar.item_a [ props "Twitter" "https://twitter.com/MangelMaxime"
                          className "twitter" ]
            [ Icon.faIcon [ ]
                [ Fa.icon Fa.I.Twitter
                  Fa.faLg ] ]
        ] |> ofList

    let tweetUrl = "https://twitter.com/intent/tweet?via=MangelMaxime&text=Thot%20is%20a%20set%20of%20several%20libraries%20for%20working%20with%20@FableCompiler%20applications"

    let private viewButton =
        Navbar.item_div [ ]
            [ Field.field_div [ Field.isGrouped ]
                [ Control.control_p [ ]
                    [ Button.button_a [ Button.customClass "twitter"
                                        Button.props [ Href tweetUrl
                                                       Target "_blank" ] ]
                        [ Icon.faIcon [ ]
                            [ Fa.icon Fa.I.Twitter
                              Fa.faLg ]
                          span [ ] [ str "Tweet" ]
                        ]
                    ]
                  Control.control_p [ ]
                    [ Button.button_a [ Button.customClass "github"
                                        Button.props [ Href "https://github.com/MangelMaxime/thot" ] ]
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

    let private navbarJson pageUrl =
        Navbar.dropdown_div [ ]
            [ Navbar.item_a [ if pageUrl = Route.Json.Decode then yield Navbar.Item.isActive
                              yield Navbar.Item.props [ Href Route.Json.Decode ] ]
                [ str "Decode" ]
              Navbar.item_a [ if pageUrl = Route.Json.Encode then yield Navbar.Item.isActive
                              yield Navbar.Item.props [ Href Route.Json.Encode ] ]
                [ str "Encode" ] ]

    let private navbarMenu pageUrl =
        Navbar.menu [ ]
            [ Navbar.item_div [ Navbar.Item.hasDropdown
                                Navbar.Item.isHoverable ]
                [ Navbar.link_div [ ]
                    [ str "Json" ]
                  navbarJson pageUrl ] ]

    let private navbarBrand =
        Navbar.brand_div [ ]
            [ Navbar.item_a [ Navbar.Item.props [ Href Route.Index ] ]
                [ Heading.p [ Heading.is4 ]
                    [ str "Thot" ] ] ]

    let render pageUrl =
        Navbar.navbar [ Navbar.isPrimary
                        Navbar.customClass "is-fixed-top" ]
            [ shadow
              Container.container [ ]
                [ navbarBrand
                  navbarMenu pageUrl
                  navbarEnd ] ]
