module Le.Block.Dashboard exposing (..)

import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Le.Api as Api
import Le.Routes


type Msg
    = NoOp
    | LogoutPressed


type alias Model =
    ()


init : Model
init =
    ()


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        LogoutPressed ->
            ( model, Nav.load Le.Routes.logout )


{-| See `Index.routeName`
-}
navbarContent : (Msg -> msg) -> String -> Maybe Api.AccountInfo -> Html msg
navbarContent toMsg routeName mAccInfo =
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
                , ul [ class "navbar-nav ml-auto" ] <|
                    case mAccInfo of
                        Nothing ->
                            [ li [ class "nav-item" ]
                                [ a
                                    [ class "nav-link"
                                    , href Le.Routes.login
                                    ]
                                    [ text "Login" ]
                                ]
                            ]

                        Just accInfo ->
                            [ li [ class "nav-item" ]
                                [ span
                                    [ class "nav-link cursor-pointer"
                                    , onClick <| toMsg LogoutPressed
                                    ]
                                    [ text <| accInfo.email ++ " (logout)" ]
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


view : (Msg -> msg) -> String -> Maybe Api.AccountInfo -> Html msg -> Html msg
view toMsg routeName mAccInfo mainContent =
    div [ class "d-flex flex-column h-100" ]
        [ navbarContent toMsg routeName mAccInfo
        , mainContent
        , footerContent
        ]
