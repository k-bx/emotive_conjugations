module Le.Pages.Login exposing (..)

import Browser.Navigation
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as J
import Le.Api as Api
import Le.Block.Toast
import Le.Lib exposing (..)
import Le.Types exposing (..)
import Le.Utils exposing (..)


type Msg
    = NoOp
    | ToastMsg Le.Block.Toast.Msg
    | UpdateModel Model
    | EmailKeyDown Int
    | CodeKeyDown Int
    | EnterEmail
    | SendCodePressed
    | EmailSent (Result Api.Error ())
    | GotLoginResponse (Result Api.Error Api.AccountInfo)


type alias Model =
    { toasts : Le.Block.Toast.Model
    , formErrors : Dict String String
    , email : String
    , code : String
    , oneTimePassword : String
    , accInfo : Maybe Api.AccountInfo
    , showCodeInput : Bool
    }


init : Browser.Navigation.Key -> ( Model, Cmd Msg )
init key =
    ( { toasts = Le.Block.Toast.init
      , formErrors = Dict.fromList []
      , email = ""
      , code = ""
      , oneTimePassword = ""
      , accInfo = Nothing
      , showCodeInput = False
      }
    , Cmd.none
    )


emailEnterPressed model =
    ( model, Api.postApiLoginsendpassword { email = model.email } EmailSent )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        codeEnterPressed =
            ( model
            , Api.postApiLogin
                { email = model.email
                , code = model.code
                }
                GotLoginResponse
            )
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ToastMsg imsg ->
            let
                ( im, icmds ) =
                    Le.Block.Toast.update imsg model.toasts
            in
            ( { model | toasts = im }, Cmd.map ToastMsg icmds )

        UpdateModel m2 ->
            ( m2, Cmd.none )

        EmailKeyDown key ->
            if key == 13 then
                emailEnterPressed model

            else
                ( model, Cmd.none )

        CodeKeyDown key ->
            if key == 13 then
                codeEnterPressed

            else
                ( model, Cmd.none )

        EnterEmail ->
            emailEnterPressed model

        SendCodePressed ->
            codeEnterPressed

        EmailSent (Err e) ->
            handleHttpError ToastMsg e model

        EmailSent (Ok ()) ->
            ( { model | showCodeInput = True }
            , Le.Block.Toast.addInfo ToastMsg "Email sent successfully. Please check your inbox"
            )

        GotLoginResponse (Err e) ->
            handleHttpError ToastMsg e model

        GotLoginResponse (Ok accInfo) ->
            let
                m2 =
                    { model | accInfo = Just accInfo }
            in
            ( { m2 | showCodeInput = False }
            , Cmd.none
            )


view : ViewParams -> Model -> Html Msg
view vp model =
    div [ class "page-login h-100 d-flex flex-column justify-content-center" ]
        [ div [ class "d-flex flex-row justify-content-center w-100" ]
            [ div [ class "login-bird d-flex flex-column justify-content-center" ]
                [ div [ class "login-form d-flex flex-row justify-content-center p-4" ] <|
                    [ div []
                        [ div [ class "d-flex flex-row justify-content-center" ] <|
                            [ label [] <|
                                [ input
                                    [ type_ "email"
                                    , class "form-control big-text-input"
                                    , isInvalidCls "email" model.formErrors
                                    , placeholder "enter email"
                                    , value model.email
                                    , onInput <| \x -> UpdateModel { model | email = x }
                                    , attribute "autocomplete" "email"
                                    , on "keydown" (J.map EmailKeyDown keyCode)
                                    ]
                                    []
                                ]
                            ]
                                ++ fieldError "email" model.formErrors
                                ++ [ span
                                        [ class "btn btn-light ml-2 send-me-code"
                                        , onClick SendCodePressed
                                        ]
                                        [ text "send me code" ]
                                   ]
                        , div [ class "social-login d-flex flex-row justify-content-center" ]
                            [ div [ class "mr-4" ]
                                [ a [ href "https://www.facebook.com/v3.2/dialog/oauth?client_id=750050712196580&redirect_uri=https%3A%2F%2Femotive-conjugations.app%2Fapi%2Ffb-login-callback&state=csrf&scope=email,public_profile" ]
                                    [ text "Sign in via Facebook" ]
                                ]
                            , div []
                                [ a [ href "https://accounts.google.com/o/oauth2/v2/auth?scope=email%20profile&response_type=code&state=csrf&redirect_uri=https%3A%2F%2Femotive-conjugations.app%2Fapi%2Fgoogle-login-callback&client_id=313436810105-et1hm4ic03bao6ru2npk2i5qjkrca5am.apps.googleusercontent.com" ]
                                    [ text "Sign in via Google" ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
