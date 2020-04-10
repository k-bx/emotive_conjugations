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
   { id: ArticleId
   , date: (Maybe IntZonedTime)
   , paper_name: String
   , title_short: String
   }

jsonDecArticleShort : Json.Decode.Decoder ( ArticleShort )
jsonDecArticleShort =
   Json.Decode.succeed (\pid pdate ppaper_name ptitle_short -> {id = pid, date = pdate, paper_name = ppaper_name, title_short = ptitle_short})
   |> required "id" (jsonDecArticleId)
   |> fnullable "date" (jsonDecIntZonedTime)
   |> required "paper_name" (Json.Decode.string)
   |> required "title_short" (Json.Decode.string)

jsonEncArticleShort : ArticleShort -> Value
jsonEncArticleShort  val =
   Json.Encode.object
   [ ("id", jsonEncArticleId val.id)
   , ("date", (maybeEncode (jsonEncIntZonedTime)) val.date)
   , ("paper_name", Json.Encode.string val.paper_name)
   , ("title_short", Json.Encode.string val.title_short)
   ]



type alias Article  =
   { id: ArticleId
   , url: String
   , date: (Maybe IntZonedTime)
   , paper_name: String
   , title: String
   , authors: (List String)
   , lang: String
   }

jsonDecArticle : Json.Decode.Decoder ( Article )
jsonDecArticle =
   Json.Decode.succeed (\pid purl pdate ppaper_name ptitle pauthors plang -> {id = pid, url = purl, date = pdate, paper_name = ppaper_name, title = ptitle, authors = pauthors, lang = plang})
   |> required "id" (jsonDecArticleId)
   |> required "url" (Json.Decode.string)
   |> fnullable "date" (jsonDecIntZonedTime)
   |> required "paper_name" (Json.Decode.string)
   |> required "title" (Json.Decode.string)
   |> required "authors" (Json.Decode.list (Json.Decode.string))
   |> required "lang" (Json.Decode.string)

jsonEncArticle : Article -> Value
jsonEncArticle  val =
   Json.Encode.object
   [ ("id", jsonEncArticleId val.id)
   , ("url", Json.Encode.string val.url)
   , ("date", (maybeEncode (jsonEncIntZonedTime)) val.date)
   , ("paper_name", Json.Encode.string val.paper_name)
   , ("title", Json.Encode.string val.title)
   , ("authors", (Json.Encode.list Json.Encode.string) val.authors)
   , ("lang", Json.Encode.string val.lang)
   ]



type alias ArticleNp  =
   { id: ArticleNpId
   , authors: (List String)
   , date: (Maybe IntZonedTime)
   , content: String
   , lang: String
   , spacy_ner_ents: (Maybe (List CmdSpacyNerResEnt))
   }

jsonDecArticleNp : Json.Decode.Decoder ( ArticleNp )
jsonDecArticleNp =
   Json.Decode.succeed (\pid pauthors pdate pcontent plang pspacy_ner_ents -> {id = pid, authors = pauthors, date = pdate, content = pcontent, lang = plang, spacy_ner_ents = pspacy_ner_ents})
   |> required "id" (jsonDecArticleNpId)
   |> required "authors" (Json.Decode.list (Json.Decode.string))
   |> fnullable "date" (jsonDecIntZonedTime)
   |> required "content" (Json.Decode.string)
   |> required "lang" (Json.Decode.string)
   |> fnullable "spacy_ner_ents" (Json.Decode.list (jsonDecCmdSpacyNerResEnt))

jsonEncArticleNp : ArticleNp -> Value
jsonEncArticleNp  val =
   Json.Encode.object
   [ ("id", jsonEncArticleNpId val.id)
   , ("authors", (Json.Encode.list Json.Encode.string) val.authors)
   , ("date", (maybeEncode (jsonEncIntZonedTime)) val.date)
   , ("content", Json.Encode.string val.content)
   , ("lang", Json.Encode.string val.lang)
   , ("spacy_ner_ents", (maybeEncode ((Json.Encode.list jsonEncCmdSpacyNerResEnt))) val.spacy_ner_ents)
   ]



type alias CmdSpacyNerResEnt  =
   { text: String
   , start: Int
   , start_char: Int
   , end: Int
   , end_char: Int
   , label_: String
   }

jsonDecCmdSpacyNerResEnt : Json.Decode.Decoder ( CmdSpacyNerResEnt )
jsonDecCmdSpacyNerResEnt =
   Json.Decode.succeed (\ptext pstart pstart_char pend pend_char plabel_ -> {text = ptext, start = pstart, start_char = pstart_char, end = pend, end_char = pend_char, label_ = plabel_})
   |> required "text" (Json.Decode.string)
   |> required "start" (Json.Decode.int)
   |> required "start_char" (Json.Decode.int)
   |> required "end" (Json.Decode.int)
   |> required "end_char" (Json.Decode.int)
   |> required "label_" (Json.Decode.string)

jsonEncCmdSpacyNerResEnt : CmdSpacyNerResEnt -> Value
jsonEncCmdSpacyNerResEnt  val =
   Json.Encode.object
   [ ("text", Json.Encode.string val.text)
   , ("start", Json.Encode.int val.start)
   , ("start_char", Json.Encode.int val.start_char)
   , ("end", Json.Encode.int val.end)
   , ("end_char", Json.Encode.int val.end_char)
   , ("label_", Json.Encode.string val.label_)
   ]



type alias Paginated item =
   { items: (List item)
   , overall_pages: Int
   }

jsonDecPaginated : Json.Decode.Decoder item -> Json.Decode.Decoder ( Paginated item )
jsonDecPaginated localDecoder_item =
   Json.Decode.succeed (\pitems poverall_pages -> {items = pitems, overall_pages = poverall_pages})
   |> required "items" (Json.Decode.list (localDecoder_item))
   |> required "overall_pages" (Json.Decode.int)

jsonEncPaginated : (item -> Value) -> Paginated item -> Value
jsonEncPaginated localEncoder_item val =
   Json.Encode.object
   [ ("items", (Json.Encode.list localEncoder_item) val.items)
   , ("overall_pages", Json.Encode.int val.overall_pages)
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
type alias ArticleId = Int
jsonDecArticleId = Json.Decode.int
jsonEncArticleId = Json.Encode.int
type alias ArticleNpId = Int
jsonDecArticleNpId = Json.Decode.int
jsonEncArticleNpId = Json.Encode.int
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



getApiArticlesshortjson : (Maybe String) -> (Result Error  ((List ArticleShort))  -> msg) -> Cmd msg
getApiArticlesshortjson query_person toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [ [ query_person
                    |> Maybe.map (Url.Builder.string "person") ]
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



getApiArticleByArticleidArticlejson : ArticleId -> (Result Error  (Article)  -> msg) -> Cmd msg
getApiArticleByArticleidArticlejson capture_article_id toMsg =
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
                    , "article"
                    , (capture_article_id |> String.fromInt)
                    , "article.json"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                leExpectJson toMsg jsonDecArticle
            , timeout =
                Nothing
            , tracker =
                Nothing
            }



getApiArticleByArticlenpidArticlenpjson : ArticleNpId -> (Result Error  (ArticleNp)  -> msg) -> Cmd msg
getApiArticleByArticlenpidArticlenpjson capture_article_np_id toMsg =
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
                    , "article"
                    , (capture_article_np_id
                       |> String.fromInt)
                    , "article-np.json"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                leExpectJson toMsg jsonDecArticleNp
            , timeout =
                Nothing
            , tracker =
                Nothing
            }



getApiPersonnamedentitieslistjson : (Maybe String) -> (Maybe Int) -> (Result Error  ((Paginated String))  -> msg) -> Cmd msg
getApiPersonnamedentitieslistjson query_q query_page toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [ [ query_q
                    |> Maybe.map (Url.Builder.string "q") ]
                , [ query_page
                    |> Maybe.map (String.fromInt
                                  >> Url.Builder.string "page") ]
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
                    , "person-named-entities-list.json"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                leExpectJson toMsg (jsonDecPaginated jsonDecText)
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

