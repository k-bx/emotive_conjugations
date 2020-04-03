module Le.Block.Toast exposing (..)

import EnumeratedList as EL exposing (EnumeratedItem, EnumeratedList(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Svg
import Svg.Attributes as Svg
import Task
import Time
import Time.Distance


type Msg
    = NoOp
    | ClosePressed (EnumeratedItem Toast)
    | AddToast { level : Level, label : String }
    | AddToastGotTime { level : Level, label : String } Time.Posix


type Level
    = Error
    | Warning
    | Info


type alias Toast =
    { level : Level
    , label : String
    , created : Time.Posix
    }


type alias Model =
    { toasts : EnumeratedList Toast
    }


init : Model
init =
    { toasts = EL.fromList [] }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ClosePressed toastEi ->
            ( { model | toasts = EL.deleteAtItem toastEi model.toasts }
            , Cmd.none
            )

        AddToast data ->
            ( model, Task.perform (AddToastGotTime data) Time.now )

        AddToastGotTime data t ->
            let
                new =
                    { label = data.label
                    , level = data.level
                    , created = t
                    }
            in
            ( { model | toasts = EL.fromList (EL.toList model.toasts ++ [ new ]) }, Cmd.none )


addToast :
    { toMsg : Msg -> msg
    , level : Level
    , label : String
    }
    -> Cmd msg
addToast ps =
    Cmd.map ps.toMsg (Task.perform (AddToastGotTime { level = ps.level, label = ps.label }) Time.now)


addError : (Msg -> msg) -> String -> Cmd msg
addError toMsg s =
    addToast { toMsg = toMsg, level = Error, label = s }


addWarning : (Msg -> msg) -> String -> Cmd msg
addWarning toMsg s =
    addToast { toMsg = toMsg, level = Warning, label = s }


addInfo : (Msg -> msg) -> String -> Cmd msg
addInfo toMsg s =
    addToast { toMsg = toMsg, level = Info, label = s }


addErrors : (Msg -> msg) -> List String -> Cmd msg
addErrors toMsg xs =
    Cmd.batch (List.map (addError toMsg) xs)


viewToast :
    (Msg -> msg)
    -> Time.Posix
    -> EnumeratedItem Toast
    -> Html msg
viewToast toMsg t toastEi =
    let
        headerLabel =
            case toastEi.element.level of
                Info ->
                    "Info"

                Warning ->
                    "Warning"

                Error ->
                    "Error"
    in
    div
        [ class "toast fade show"
        , classList
            [ ( "toast--info", toastEi.element.level == Info )
            , ( "toast--warning", toastEi.element.level == Warning )
            , ( "toast--error", toastEi.element.level == Error )
            ]
        , attribute "role" "alert"
        , attribute "aria-live" "assertive"
        , attribute "aria-atomic" "true"
        ]
        [ div [ class "toast-header" ]
            [ strong [ class "mr-auto" ] [ text headerLabel ]
            , button
                [ type_ "button"
                , class "ml-2 mb-1 close"
                , attribute "data-dismiss" "toast"
                , attribute "aria-label" "Close"
                ]
                [ span
                    [ attribute "aria-hidden" "true"
                    , onClick (toMsg (ClosePressed toastEi))
                    ]
                    [ i [ class "fa fa-times" ] [] ]
                ]
            ]
        , div [ class "toast-body" ]
            [ div [] [ text toastEi.element.label ]
            , div []
                [ small []
                    [ let
                        createdAt =
                            if Time.posixToMillis toastEi.element.created >= Time.posixToMillis t then
                                Time.millisToPosix (Time.posixToMillis toastEi.element.created - 5000)

                            else
                                toastEi.element.created
                      in
                      text <|
                        Time.Distance.inWords createdAt t
                    ]
                ]
            ]
        ]


view : (Msg -> msg) -> Maybe Time.Posix -> Model -> Html msg -> Html msg
view toMsg mt model inner =
    case mt of
        Nothing ->
            inner

        Just t ->
            div []
                [ div [ class "toasts" ]
                    [ div [ class "toasts__wrap" ]
                        (EL.toList (EL.map (viewToast toMsg t) model.toasts))
                    ]
                , inner
                ]
