[<RequireQualifiedAccess>]
module Components.Navbar

    open Fulma.Components
    open Fable.Helpers.React
    open Fable.Helpers.React.Props
    open Fulma
    open Fulma.Layouts
    open Fulma.Elements
    open Fulma.Elements.Form
    open Fulma.Extra.FontAwesome

    let private shadow =
        div [ ClassName "bd-special-shadow"
              Style [ Opacity 1.
                      Transform "scaleY(1)" ] ]
            [ ]

    let private viewSimpleIcon isHiddenDesktop =
        let inline props key url = Navbar.Item.Props [ Key ("simple-icon-"+ key)
                                                       Href url ]

        let visibility =
            if isHiddenDesktop then
                "is-hidden-desktop"
            else
                "is-hidden-mobile"

        [ Navbar.Item.a [ props "Github" "https://github.com/MangelMaxime/thot"
                          Navbar.Item.CustomClass visibility ]
            [ Icon.faIcon [ ]
                [ Fa.icon Fa.I.Github
                  Fa.faLg ] ]
          Navbar.Item.a [ props "Twitter" "https://twitter.com/MangelMaxime"
                          Navbar.Item.CustomClass ("twitter " + visibility) ]
            [ Icon.faIcon [ ]
                [ Fa.icon Fa.I.Twitter
                  Fa.faLg ] ]
        ] |> ofList

    let tweetUrl = "https://twitter.com/intent/tweet?via=MangelMaxime&text=Thot%20is%20a%20set%20of%20several%20libraries%20for%20working%20with%20@FableCompiler%20applications"

    let private viewButton =
        Navbar.Item.div [ ]
            [ Field.div [ Field.IsGrouped ]
                [ Control.p [ ]
                    [ Button.a [ Button.CustomClass "twitter"
                                 Button.Props [ Href tweetUrl
                                                Target "_blank" ] ]
                        [ Icon.faIcon [ ]
                            [ Fa.icon Fa.I.Twitter
                              Fa.faLg ]
                          span [ ] [ str "Tweet" ]
                        ]
                    ]
                  Control.p [ ]
                    [ Button.a [ Button.CustomClass "github"
                                 Button.Props [ Href "https://github.com/MangelMaxime/thot" ] ]
                        [ Icon.faIcon [ ]
                            [ Fa.icon Fa.I.Github
                              Fa.faLg ]
                          span [ ] [ str "Github" ]
                        ]
                    ]
                ]
            ]

    let private navbarEnd =
        Navbar.End.div [ ]
            [ viewSimpleIcon false
              viewButton ]

    let private navbarJson pageUrl =
        Navbar.Dropdown.div [ ]
            [ Navbar.Item.a [ Navbar.Item.IsActive (pageUrl = Route.Json.Decode)
                              Navbar.Item.Props [ Href Route.Json.Decode ] ]
                [ str "Decode" ]
              Navbar.Item.a [ Navbar.Item.IsActive (pageUrl = Route.Json.Encode )
                              Navbar.Item.Props [ Href Route.Json.Encode ] ]
                [ str "Encode" ] ]

    let private navbarHttp pageUrl =
        Navbar.Dropdown.div [ ]
            [ Navbar.Item.a [ Navbar.Item.IsActive (pageUrl = Route.Http.Basic)
                              Navbar.Item.Props [ Href Route.Http.Basic ] ]
                [ str "Basic" ]
              Navbar.Item.a [ Navbar.Item.IsActive (pageUrl = Route.Http.Elmish)
                              Navbar.Item.Props [ Href Route.Http.Basic ] ]
                [ str "Elmish usage" ] ]

    let private navbarMenu pageUrl =
        Navbar.menu [ Navbar.Menu.Props [ Id "navMenu" ] ]
            [ Navbar.Item.div [ Navbar.Item.HasDropdown
                                Navbar.Item.IsHoverable ]
                [ Navbar.Link.div [ ]
                    [ str "Json" ]
                  navbarJson pageUrl ]
              Navbar.Item.div [ Navbar.Item.HasDropdown
                                Navbar.Item.IsHoverable ]
                [ Navbar.Link.div [ ]
                    [ str "Http" ]
                  navbarHttp pageUrl ] ]

    let private navbarBrand =
        Navbar.Brand.div [ ]
            [ Navbar.Item.a [ Navbar.Item.Props [ Href Route.Index ] ]
                [ Heading.p [ Heading.Is4 ]
                    [ str "Thot" ] ]
              viewSimpleIcon true
              Navbar.burger [ ]
                [ span [ ] [ ]
                  span [ ] [ ]
                  span [ ] [ ] ] ]

    let render pageUrl =
        Navbar.navbar [ Navbar.Color IsPrimary
                        Navbar.CustomClass "is-fixed-top" ]
            [ shadow
              Container.container [ ]
                [ navbarBrand
                  navbarMenu pageUrl
                  navbarEnd ] ]
