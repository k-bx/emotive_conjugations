module Le.Pages.Login exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Le.Api as Api
import Le.Block.Toast


type Msg
    = NoOp
    | ToastMsg Le.Block.Toast.Msg
    | EmailKeyDown Int
    | CodeKeyDown Int
    | EnterEmail
    | EnterCode
    | EmailSent (Result Api.Error ())
    | GotLoginResponse (Result Api.Error Api.AccountInfo)


type alias Model =
    { toasts : Le.Block.Toast.Model
    , formErrors : Dict String String
    , email : String
    , code : String
    , oneTimePassword : String
    , accInfo : Maybe Api.AccountInfo
    }


init : Model
init =
    { toasts = Le.Block.Toast.init
    , formErrors = Dict.fromList []
    , email = ""
    , code = ""
    , oneTimePassword = ""
    , accInfo = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        emailEnterPressed =
            ( Api.postApiLoginsendpassword { email = model.email } EmailSent
            )

        codeEnterPressed =
            ( incLoading model
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

        EmailKeyDown key ->
            if key == 13 then
                emailEnterPressed

            else
                ( model, Cmd.none )

        CodeKeyDown key ->
            if key == 13 then
                codeEnterPressed

            else
                ( model, Cmd.none )

        EnterEmail ->
            emailEnterPressed

        EmailChange v ->
            ( { model | email = v }, Cmd.none )

        CodeChange v ->
            ( { model | code = v }, Cmd.none )

        EnterCode ->
            codeEnterPressed

        EmailSent (Err e) ->
            ( { model | loading = model.loading - 1 }
            , Le.Block.Toast.addError ToastMsg "Error while sending one-time code, please try again"
            )

        EmailSent (Ok ()) ->
            ( handleHttpSuccess { model | showCodeInput = True }
            , Le.Block.Toast.addInfo ToastMsg "Email sent successfully. Please check your inbox"
            )

        GotLoginResponse (Err e) ->
            ( { model | loading = model.loading - 1 }
            , Le.Block.Toast.addError ToastMsg "Error while trying to log in"
            )

        GotLoginResponse (Ok accInfo) ->
            let
                m2 =
                    fillAccInfo accInfo model
            in
            ( handleHttpSuccess { m2 | showCodeInput = False }
            , Cmd.none
            )

        GotAccountInfo (Err e) ->
            handleHttpErrorNoRedirect ToastMsg e model

        GotAccountInfo (Ok accInfo) ->
            ( handleHttpSuccess { model | accInfo = Just accInfo }
            , Cmd.none
            )

        FullNameChange v ->
            ( { model | fullName = v }, Cmd.none )

        PhoneNumberChange v ->
            ( { model | phoneNumber = v }, Cmd.none )

        SaveProfileDetails ->
            ( incLoading model
            , Api.postApiAccountUpdatejson
                { full_name = model.fullName
                , phone_number = model.phoneNumber
                }
                UpdateAccountDone
            )

        UpdateAccountDone (Err e) ->
            handleHttpErrorNoRedirect ToastMsg e model

        UpdateAccountDone (Ok accInfo) ->
            -- handled by parent update
            ( handleHttpSuccess { model | accInfo = Just accInfo }
            , Cmd.none
            )
