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


view : ViewParams -> Model -> Html Msg
view vps model =
    Le.Block.Toast.view ToastMsg vps.now model.toasts <|
        div []
            [ text "hello, dashboard"
            ]
