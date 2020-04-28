module Le.Pages.Queue exposing (..)

import Browser.Navigation
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Le.Api as Api
import Le.Article
import Le.Block.Dashboard
import Le.Block.Toast
import Le.Components exposing (..)
import Le.Config
import Le.Lib exposing (..)
import Le.Ports
import Le.Routes
import Le.Spacy
import Le.Types exposing (..)
import Le.Utils exposing (..)
import Maybe.Extra
import Process
import SelectTwo
import SelectTwo.Html
import SelectTwo.Types exposing (AjaxParams, SelectTwo, SelectTwoMsg)
import Set
import Task
import Time


type Msg
    = NoOp
    | ToastMsg Le.Block.Toast.Msg
    | UpdateModel Model
    | GotAccount (Result Api.Error Api.AccountInfo)
    | DashboardMsg Le.Block.Dashboard.Msg


type alias Model =
    { formErrors : Dict String String
    , toasts : Le.Block.Toast.Model
    , key : Browser.Navigation.Key
    , accInfo : Maybe Api.AccountInfo
    , dashboard : Le.Block.Dashboard.Model
    }


init : Browser.Navigation.Key -> ( Model, Cmd Msg )
init key =
    ( { formErrors = Dict.empty
      , toasts = Le.Block.Toast.init
      , key = key
      , accInfo = Nothing
      , dashboard = Le.Block.Dashboard.init
      }
    , Api.getApiAccountinfojson GotAccount
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

        DashboardMsg imsg ->
            let
                ( im2, icmds ) =
                    Le.Block.Dashboard.update imsg model.dashboard
            in
            ( { model | dashboard = im2 }, Cmd.map DashboardMsg icmds )

        GotAccount (Err e) ->
            handleHttpError ToastMsg e model

        GotAccount (Ok accInfo) ->
            ( { model | accInfo = Just accInfo }
            , Cmd.none
            )


mainContent : Model -> Html Msg
mainContent model =
    main_ [ class "main-content", attribute "role" "main" ]
        [ div [ class "container" ]
            [ div [ class "row" ]
                [ div [ class "col-12" ]
                    [ text "ui here"
                    ]
                ]
            ]
        ]


view : ViewParams -> Model -> Html Msg
view vps model =
    Le.Block.Toast.view ToastMsg vps.now model.toasts <|
        div
            [ class "h-100 gr__getbootstrap_com page-queue"

            -- , SelectTwo.Html.select2Close SelectTwo
            ]
            [ Le.Block.Dashboard.view DashboardMsg vps.routeName model.accInfo <|
                div []
                    [ mainContent model
                    ]

            -- , SelectTwo.Html.select2Dropdown SelectTwo Nothing model
            ]
