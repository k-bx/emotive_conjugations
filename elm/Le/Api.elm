module Le.Api exposing (..)

{-| WARNING! This file is auto-generated, don't edit it manually.

See `make generate-elm` and `GenElm.hs` for details.
-}

import Http
import Json.Decode exposing (Value)
import Json.Encode
import Json.Helpers exposing (required, fnullable, maybeEncode, decodeSumUnaries)
import Dict exposing (Dict)
import Url.Builder

type alias Text = String
jsonDecText = Json.Decode.string

type alias Tuple2 a b = { t2f1 : a, t2f2 : b }

-- tuple2 a b = { t2f1 = a, t2f2 = b }

jsonEncTuple2 : (a -> Value) -> (b -> Value) -> Tuple2 a b -> Value
jsonEncTuple2  enc1 enc2 tpl =
   Json.Encode.object
   [ ("1", enc1 tpl.t2f1)
   , ("2", enc2 tpl.t2f2)
   ]

jsonDecTuple2 : Json.Decode.Decoder dec1 -> Json.Decode.Decoder dec2 -> Json.Decode.Decoder (Tuple2 dec1 dec2)
jsonDecTuple2 dec1 dec2 =
   Json.Decode.succeed (\pv1 pv2 -> {t2f1 = pv1, t2f2 = pv2})
   |> required "1" dec1
   |> required "2" dec2

jsonPair : Json.Decode.Decoder a -> Json.Decode.Decoder b -> Json.Decode.Decoder ( a, b )
jsonPair a b =
    Json.Decode.andThen
        (\xs ->
            case xs of
                [] ->
                    Json.Decode.fail "Expecting a list of two elements"

                x :: y :: [] ->
                    case ( Json.Decode.decodeValue a x, Json.Decode.decodeValue b y ) of
                        ( Ok av, Ok bv ) ->
                            Json.Decode.succeed ( av, bv )

                        _ ->
                            Json.Decode.fail "Error while decoding individual pair fields"

                _ ->
                    Json.Decode.fail "Expecting a list of two elements"
        )
        (Json.Decode.list Json.Decode.value)

type alias Error =
    { httpError : Http.Error
    , formErrors : Dict String String
    , errors : List String
    }

leExpectJson : (Result Error a -> msg) -> Json.Decode.Decoder a -> Http.Expect msg
leExpectJson toMsg decoder =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err { httpError = Http.BadUrl url, formErrors = Dict.empty, errors = [] }

                Http.Timeout_ ->
                    Err { httpError = Http.Timeout, formErrors = Dict.empty, errors = [] }

                Http.NetworkError_ ->
                    Err { httpError = Http.NetworkError, formErrors = Dict.empty, errors = [] }

                Http.BadStatus_ metadata body ->
                    case metadata.statusCode of
                        400 ->
                            case Json.Decode.decodeString (Json.Decode.list (jsonPair Json.Decode.string Json.Decode.string)) body of
                                Err decodeErr ->
                                    Err
                                        { httpError = Http.BadStatus metadata.statusCode
                                        , errors = [ "Error while decoding a back-end response: " ++ Json.Decode.errorToString decodeErr ]
                                        , formErrors = Dict.empty
                                        }

                                Ok vs ->
                                    Err
                                        { httpError = Http.BadStatus metadata.statusCode
                                        , errors = []
                                        , formErrors = Dict.fromList vs
                                        }

                        _ ->
                            Err { httpError = Http.BadStatus metadata.statusCode, formErrors = Dict.empty, errors = ["There was a back-end error. Try again or contact administrators"] }

                Http.GoodStatus_ metadata body ->
                    case Json.Decode.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err { httpError = Http.BadBody (Json.Decode.errorToString err), formErrors = Dict.empty, errors = [] }


type alias NoOp  =
   { field: String
   }

jsonDecNoOp : Json.Decode.Decoder ( NoOp )
jsonDecNoOp =
   Json.Decode.succeed (\pfield -> {field = pfield})
   |> required "field" (Json.Decode.string)

jsonEncNoOp : NoOp -> Value
jsonEncNoOp  val =
   Json.Encode.object
   [ ("field", Json.Encode.string val.field)
   ]



type alias ArticleShort  =
   { date: IntZonedTime
   , paper_name: String
   , title_short: String
   }

jsonDecArticleShort : Json.Decode.Decoder ( ArticleShort )
jsonDecArticleShort =
   Json.Decode.succeed (\pdate ppaper_name ptitle_short -> {date = pdate, paper_name = ppaper_name, title_short = ptitle_short})
   |> required "date" (jsonDecIntZonedTime)
   |> required "paper_name" (Json.Decode.string)
   |> required "title_short" (Json.Decode.string)

jsonEncArticleShort : ArticleShort -> Value
jsonEncArticleShort  val =
   Json.Encode.object
   [ ("date", jsonEncIntZonedTime val.date)
   , ("paper_name", Json.Encode.string val.paper_name)
   , ("title_short", Json.Encode.string val.title_short)
   ]


type alias IntUTCTime = Int
jsonDecIntUTCTime = Json.Decode.int
jsonEncIntUTCTime = Json.Encode.int
type alias IntZonedTime = Int
jsonDecIntZonedTime = Json.Decode.int
jsonEncIntZonedTime = Json.Encode.int
type alias Milliseconds = Int
jsonDecMilliseconds = Json.Decode.int
jsonEncMilliseconds = Json.Encode.int
type alias CompanyId = Int
jsonDecCompanyId = Json.Decode.int
jsonEncCompanyId = Json.Encode.int
type alias DayString = String
jsonDecDayString = Json.Decode.string
jsonEncDayString = Json.Encode.string
type alias TimeZoneName = String
jsonDecTimeZoneName = Json.Decode.string
jsonEncTimeZoneName = Json.Encode.string
type alias TimeZoneUILabel = String
jsonDecTimeZoneUILabel = Json.Decode.string
jsonEncTimeZoneUILabel = Json.Encode.string
type alias TZLabel = String
jsonDecTZLabel = Json.Decode.string
jsonEncTZLabel = Json.Encode.string
type alias S3Loc = String
jsonDecS3Loc = Json.Decode.string
jsonEncS3Loc = Json.Encode.string
type alias Scientific = Float
jsonDecScientific = Json.Decode.float
jsonEncScientific = Json.Encode.float
getApiLogerrorjson : (Maybe String) -> (Result Error  (())  -> msg) -> Cmd msg
getApiLogerrorjson query_msg toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [ [ query_msg
                    |> Maybe.map (Url.Builder.string "msg") ]
                ])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin ""
                    [ "api"
                    , "log-error.json"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectString 
                     (\x -> case x of
                     Err e -> toMsg (Err {httpError=e,formErrors=Dict.empty,errors=[]})
                     Ok _ -> toMsg (Ok ()))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }



getApiPingjson : (Result Error  ((List String))  -> msg) -> Cmd msg
getApiPingjson toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin ""
                    [ "api"
                    , "ping.json"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                leExpectJson toMsg (Json.Decode.list (jsonDecText))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }



getApiErroroutjson : (Result Error  ((List String))  -> msg) -> Cmd msg
getApiErroroutjson toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin ""
                    [ "api"
                    , "error-out.json"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                leExpectJson toMsg (Json.Decode.list (jsonDecText))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }



getApiArticlesshortjson : (Result Error  ((List ArticleShort))  -> msg) -> Cmd msg
getApiArticlesshortjson toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin ""
                    [ "api"
                    , "articles-short.json"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                leExpectJson toMsg (Json.Decode.list (jsonDecArticleShort))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

