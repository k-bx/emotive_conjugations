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
    | GotQueue (Result Api.Error (List Api.QueueItem))
    | AddUrlPressed
    | QueueItemAdded (Result Api.Error ())


type alias Model =
    { formErrors : Dict String String
    , toasts : Le.Block.Toast.Model
    , key : Browser.Navigation.Key
    , accInfo : Maybe Api.AccountInfo
    , dashboard : Le.Block.Dashboard.Model
    , queue : Maybe (List Api.QueueItem)
    , url : String
    }


init : Browser.Navigation.Key -> ( Model, Cmd Msg )
init key =
    ( { formErrors = Dict.empty
      , toasts = Le.Block.Toast.init
      , key = key
      , accInfo = Nothing
      , dashboard = Le.Block.Dashboard.init
      , queue = Nothing
      , url = ""
      }
    , Cmd.batch
        [ Api.getApiAccountinfojson GotAccount
        , Api.getApiQueuejson GotQueue
        ]
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

        GotQueue (Err e) ->
            handleHttpError ToastMsg e model

        GotQueue (Ok queue) ->
            ( { model | queue = Just queue }
            , Cmd.none
            )

        AddUrlPressed ->
            let
                form : Api.QueueAddForm
                form =
                    { url = model.url }
            in
            ( model
            , Api.postApiQueueAddjson form QueueItemAdded
            )

        QueueItemAdded (Err e) ->
            handleHttpError ToastMsg e model

        QueueItemAdded (Ok ()) ->
            ( model
            , Api.getApiQueuejson GotQueue
            )


mainContent : Model -> Html Msg
mainContent model =
    let
        queueBlock =
            div [ class "queue" ]
                [ div [ class "queue__add d-flex flex-row justify-content-center mb-4" ]
                    [ label [] <|
                        [ input
                            [ type_ "text"
                            , class "form-control big-text-input queue__add__input"
                            , isInvalidCls "url" model.formErrors
                            , placeholder "https://www.nytimes.com/2020/04/29/us/politics/coronavirus-trump-justice-department.html"
                            , value model.url
                            , onInput <| \x -> UpdateModel { model | url = x }
                            ]
                            []
                        ]
                            ++ fieldError "url" model.formErrors
                    , span
                        [ class "btn btn-light big-button ml-2"
                        , onClick AddUrlPressed
                        ]
                        [ text "add url" ]
                    ]
                , div [ class "queue__items" ] <|
                    case model.queue of
                        Nothing ->
                            [ loadingSpinner ]

                        Just queue ->
                            renderQueue queue
                ]

        renderQueue queue =
            List.map renderQueueItem queue

        renderQueueItem queueItem =
            let
                statusBadgeClass =
                    if queueItem.errored then
                        "badge-error"

                    else if queueItem.status == Api.QueueItemStatusDone then
                        "badge-success"

                    else
                        "badge-info"

                statusBadge =
                    span
                        [ class "badge"
                        , class statusBadgeClass
                        ]
                        [ text <| "status:" ++ Api.stringEncQueueItemStatus queueItem.status
                        ]
            in
            div [ class "queue-item d-flex flex-row justify-content-between mb-2" ]
                [ div [ class "queue-item__status-cell flex-grow-0 p-2" ]
                    [ div []
                        [ span [ class "badge badge-info" ]
                            [ text <| "id:" ++ String.fromInt queueItem.id
                            ]
                        , span [ class "ml-1" ] [ statusBadge ]
                        ]
                    , div []
                        [ span [ class "badge badge-info" ]
                            [ text <| "created-at:" ++ (renderDateInfobox << Time.millisToPosix) queueItem.created_at
                            ]
                        ]
                    ]
                , div [ class "queue-item__details-cell flex-grow-1 p-2" ]
                    [ a
                        [ target "_blank"
                        , href queueItem.url
                        ]
                        [ text queueItem.url ]
                    ]
                ]
    in
    main_ [ class "main-content", attribute "role" "main" ]
        [ div [ class "container" ]
            [ div [ class "row" ]
                [ div [ class "col-12" ]
                    [ queueBlock
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
