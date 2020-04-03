module Le.Lib exposing (..)

import Dict exposing (Dict)
import Http
import Json.Decode as J
import Json.Encode as Json
import Le.Api as Api
import Le.Block.Toast
import Le.Ports as Ports


type alias ModelWithErrorFields model =
    { model
        | formErrors : Dict String String
        , toasts : Le.Block.Toast.Model
    }


handleHttpError : (Le.Block.Toast.Msg -> msg) -> Api.Error -> ModelWithErrorFields model -> ( ModelWithErrorFields model, Cmd msg )
handleHttpError toMsg e m =
    case e.httpError of
        Http.BadStatus code ->
            case code of
                401 ->
                    ( m, Ports.needLoginRedirect () )

                403 ->
                    ( m, Ports.needLoginRedirect () )

                _ ->
                    case Dict.toList e.formErrors of
                        [] ->
                            case e.errors of
                                [] ->
                                    ( { m | formErrors = e.formErrors }
                                    , Le.Block.Toast.addError toMsg "There was an error, please try again later"
                                    )

                                es ->
                                    ( { m | formErrors = e.formErrors }
                                    , Le.Block.Toast.addErrors toMsg es
                                    )

                        _ ->
                            ( { m | formErrors = e.formErrors }
                            , Cmd.none
                            )

        Http.BadBody badBody ->
            ( m
            , Cmd.batch
                [ Le.Block.Toast.addError toMsg "There was an error, please try again later"
                , Api.getApiLogerrorjson (Just ("Bad Body: " ++ badBody)) (toMsg << always Le.Block.Toast.NoOp)
                ]
            )

        _ ->
            ( m
            , Le.Block.Toast.addError toMsg "There was an error, please try again later"
            )


handleHttpErrorNoRedirect : (Le.Block.Toast.Msg -> msg) -> Api.Error -> ModelWithErrorFields model -> ( ModelWithErrorFields model, Cmd msg )
handleHttpErrorNoRedirect toMsg e m =
    case e.httpError of
        Http.BadStatus code ->
            case code of
                401 ->
                    ( m, Cmd.none )

                403 ->
                    ( m, Cmd.none )

                _ ->
                    case Dict.toList e.formErrors of
                        [] ->
                            case e.errors of
                                [] ->
                                    ( { m | formErrors = e.formErrors }
                                    , Le.Block.Toast.addError toMsg "There was an error, please try again later"
                                    )

                                es ->
                                    ( { m | formErrors = e.formErrors }
                                    , Le.Block.Toast.addErrors toMsg es
                                    )

                        _ ->
                            ( { m | formErrors = e.formErrors }
                            , Cmd.none
                            )

        Http.BadBody badBody ->
            ( m
            , Cmd.batch
                [ Le.Block.Toast.addError toMsg "There was an error, please try again later"
                , Api.getApiLogerrorjson (Just ("Bad Body: " ++ badBody)) (toMsg << always Le.Block.Toast.NoOp)
                ]
            )

        _ ->
            ( m
            , Le.Block.Toast.addError toMsg "There was an error, please try again later"
            )


fromTuple2 : Api.Tuple2 a b -> ( a, b )
fromTuple2 x =
    ( x.t2f1, x.t2f2 )
