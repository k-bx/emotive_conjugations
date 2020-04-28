module Le.Block.Dashboard exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Le.Routes


type Msg
    = NoOp


{-| See `Index.routeName`
-}
navbarContent : String -> Html msg
navbarContent routeName =
    header []
        [ nav [ class "navbar navbar-expand-md navbar-dark bg-dark" ]
            [ a
                [ class "navbar-brand"
                , href <| Le.Routes.dashboard "" Nothing
                ]
                [ img
                    [ src "/images/blue-tailed-bee-eater.svg"
                    , class "logo mr-3"
                    ]
                    []
                , text "Emotive Conjugations"
                ]
            , button [ attribute "aria-controls" "navbarCollapse", attribute "aria-expanded" "false", attribute "aria-label" "Toggle navigation", class "navbar-toggler", attribute "data-target" "#navbarCollapse", attribute "data-toggle" "collapse", type_ "button" ]
                [ span [ class "navbar-toggler-icon" ]
                    []
                ]
            , div [ class "collapse navbar-collapse", id "navbarCollapse" ]
                [ ul [ class "navbar-nav mr-auto" ]
                    [ li
                        [ class "nav-item"
                        , classList [ ( "active", routeName == "dashboard" ) ]
                        ]
                        [ a
                            [ class "nav-link"
                            , href <| Le.Routes.dashboard "" Nothing
                            ]
                            [ text "Dashboard"
                            ]
                        ]
                    , li
                        [ class "nav-item"
                        , classList [ ( "active", routeName == "queue" ) ]
                        ]
                        [ a
                            [ class "nav-link"
                            , href <| Le.Routes.queue
                            ]
                            [ text "Queue"
                            ]
                        ]
                    ]
                ]
            ]
        ]


footerContent : Html msg
footerContent =
    footer [ class "footer mt-auto py-3" ]
        [ div [ class "text-center" ]
            [ span [ class "text-muted" ]
                [ text "Made by "
                , a [ href "https://k-bx.github.io", target "_blank" ] [ text "Konstantine Rybnikov" ]
                , text " with help from "
                , a [ href "https://theportal.wiki/", target "_blank" ] [ text "The Portal" ]
                , text " community"
                ]
            ]
        ]


view : String -> Html msg -> Html msg
view routeName mainContent =
    div [ class "d-flex flex-column h-100" ]
        [ navbarContent routeName
        , mainContent
        , footerContent
        ]
