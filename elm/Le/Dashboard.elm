module Le.Dashboard exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Le.Api as Api
import Le.Block.Toast
import Le.Config
import Le.Lib exposing (..)
import Le.Routes
import Le.Types exposing (..)
import Le.Utils exposing (..)
import Maybe.Extra
import Process
import Task


type Msg
    = NoOp
    | ToastMsg Le.Block.Toast.Msg
    | UpdateModel Model


type alias Model =
    { formErrors : Dict String String
    , toasts : Le.Block.Toast.Model
    }


init : ( Model, Cmd Msg )
init =
    ( { formErrors = Dict.empty
      , toasts = Le.Block.Toast.init
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateModel m2 ->
            ( m2, Cmd.none )

        ToastMsg imsg ->
            let
                ( im, icmds ) =
                    Le.Block.Toast.update imsg model.toasts
            in
            ( { model | toasts = im }, Cmd.map ToastMsg icmds )


navbar : Html Msg
navbar =
    header []
        [ nav [ class "navbar navbar-expand-md navbar-dark fixed-top bg-dark" ]
            [ a [ class "navbar-brand", href "#" ]
                [ text "Emotive Conjugations" ]
            , button [ attribute "aria-controls" "navbarCollapse", attribute "aria-expanded" "false", attribute "aria-label" "Toggle navigation", class "navbar-toggler", attribute "data-target" "#navbarCollapse", attribute "data-toggle" "collapse", type_ "button" ]
                [ span [ class "navbar-toggler-icon" ]
                    []
                ]
            , div [ class "collapse navbar-collapse", id "navbarCollapse" ]
                [ ul [ class "navbar-nav mr-auto" ]
                    [ li [ class "nav-item active" ]
                        [ a [ class "nav-link", href "#" ]
                            [ text "Dashboard"
                            ]
                        ]

                    -- , li [ class "nav-item" ]
                    --     [ a [ class "nav-link", href "#" ]
                    --         [ text "Link" ]
                    --     ]
                    -- , li [ class "nav-item" ]
                    --     [ a [ attribute "aria-disabled" "true", class "nav-link disabled", href "#", attribute "tabindex" "-1" ]
                    --         [ text "Disabled" ]
                    --     ]
                    -- ]
                    -- , Html.form [ class "form-inline mt-2 mt-md-0" ]
                    --     [ input [ attribute "aria-label" "Search", class "form-control mr-sm-2", placeholder "Search", type_ "text" ]
                    --         []
                    --     , button [ class "btn btn-outline-success my-2 my-sm-0", type_ "submit" ]
                    --         [ text "Search" ]
                    --     ]
                    ]
                ]
            ]
        ]


view : ViewParams -> Model -> Html Msg
view vps model =
    Le.Block.Toast.view ToastMsg vps.now model.toasts <|
        div []
            [ navbar
            , text "hello, dashboard"
            ]
