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
   , warc_id: (Maybe String)
   }

jsonDecArticle : Json.Decode.Decoder ( Article )
jsonDecArticle =
   Json.Decode.succeed (\pid purl pdate ppaper_name ptitle pauthors plang pwarc_id -> {id = pid, url = purl, date = pdate, paper_name = ppaper_name, title = ptitle, authors = pauthors, lang = plang, warc_id = pwarc_id})
   |> required "id" (jsonDecArticleId)
   |> required "url" (Json.Decode.string)
   |> fnullable "date" (jsonDecIntZonedTime)
   |> required "paper_name" (Json.Decode.string)
   |> required "title" (Json.Decode.string)
   |> required "authors" (Json.Decode.list (Json.Decode.string))
   |> required "lang" (Json.Decode.string)
   |> fnullable "warc_id" (Json.Decode.string)

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
   , ("warc_id", (maybeEncode (Json.Encode.string)) val.warc_id)
   ]



type alias ArticlePlease  =
   { id: ArticlePleaseId
   , authors: (List String)
   , date_download: (Maybe IntZonedTime)
   , date_publish: (Maybe IntZonedTime)
   , date_modify: (Maybe IntZonedTime)
   , language: (Maybe String)
   }

jsonDecArticlePlease : Json.Decode.Decoder ( ArticlePlease )
jsonDecArticlePlease =
   Json.Decode.succeed (\pid pauthors pdate_download pdate_publish pdate_modify planguage -> {id = pid, authors = pauthors, date_download = pdate_download, date_publish = pdate_publish, date_modify = pdate_modify, language = planguage})
   |> required "id" (jsonDecArticlePleaseId)
   |> required "authors" (Json.Decode.list (Json.Decode.string))
   |> fnullable "date_download" (jsonDecIntZonedTime)
   |> fnullable "date_publish" (jsonDecIntZonedTime)
   |> fnullable "date_modify" (jsonDecIntZonedTime)
   |> fnullable "language" (Json.Decode.string)

jsonEncArticlePlease : ArticlePlease -> Value
jsonEncArticlePlease  val =
   Json.Encode.object
   [ ("id", jsonEncArticlePleaseId val.id)
   , ("authors", (Json.Encode.list Json.Encode.string) val.authors)
   , ("date_download", (maybeEncode (jsonEncIntZonedTime)) val.date_download)
   , ("date_publish", (maybeEncode (jsonEncIntZonedTime)) val.date_publish)
   , ("date_modify", (maybeEncode (jsonEncIntZonedTime)) val.date_modify)
   , ("language", (maybeEncode (Json.Encode.string)) val.language)
   ]



type alias ArticlePleaseBig  =
   { id: ArticlePleaseBigId
   , maintext: String
   , spacy_ner_ents: (Maybe (List CmdSpacyNerResEnt))
   , title_spacy_ner_ents: (Maybe (List CmdSpacyNerResEnt))
   , spacy_pos_ents: (Maybe (List SpacyToken))
   , title_spacy_pos_ents: (Maybe (List SpacyToken))
   }

jsonDecArticlePleaseBig : Json.Decode.Decoder ( ArticlePleaseBig )
jsonDecArticlePleaseBig =
   Json.Decode.succeed (\pid pmaintext pspacy_ner_ents ptitle_spacy_ner_ents pspacy_pos_ents ptitle_spacy_pos_ents -> {id = pid, maintext = pmaintext, spacy_ner_ents = pspacy_ner_ents, title_spacy_ner_ents = ptitle_spacy_ner_ents, spacy_pos_ents = pspacy_pos_ents, title_spacy_pos_ents = ptitle_spacy_pos_ents})
   |> required "id" (jsonDecArticlePleaseBigId)
   |> required "maintext" (Json.Decode.string)
   |> fnullable "spacy_ner_ents" (Json.Decode.list (jsonDecCmdSpacyNerResEnt))
   |> fnullable "title_spacy_ner_ents" (Json.Decode.list (jsonDecCmdSpacyNerResEnt))
   |> fnullable "spacy_pos_ents" (Json.Decode.list (jsonDecSpacyToken))
   |> fnullable "title_spacy_pos_ents" (Json.Decode.list (jsonDecSpacyToken))

jsonEncArticlePleaseBig : ArticlePleaseBig -> Value
jsonEncArticlePleaseBig  val =
   Json.Encode.object
   [ ("id", jsonEncArticlePleaseBigId val.id)
   , ("maintext", Json.Encode.string val.maintext)
   , ("spacy_ner_ents", (maybeEncode ((Json.Encode.list jsonEncCmdSpacyNerResEnt))) val.spacy_ner_ents)
   , ("title_spacy_ner_ents", (maybeEncode ((Json.Encode.list jsonEncCmdSpacyNerResEnt))) val.title_spacy_ner_ents)
   , ("spacy_pos_ents", (maybeEncode ((Json.Encode.list jsonEncSpacyToken))) val.spacy_pos_ents)
   , ("title_spacy_pos_ents", (maybeEncode ((Json.Encode.list jsonEncSpacyToken))) val.title_spacy_pos_ents)
   ]



type alias NamedEntityGroup  =
   { entity: String
   , group: (List String)
   }

jsonDecNamedEntityGroup : Json.Decode.Decoder ( NamedEntityGroup )
jsonDecNamedEntityGroup =
   Json.Decode.succeed (\pentity pgroup -> {entity = pentity, group = pgroup})
   |> required "entity" (Json.Decode.string)
   |> required "group" (Json.Decode.list (Json.Decode.string))

jsonEncNamedEntityGroup : NamedEntityGroup -> Value
jsonEncNamedEntityGroup  val =
   Json.Encode.object
   [ ("entity", Json.Encode.string val.entity)
   , ("group", (Json.Encode.list Json.Encode.string) val.group)
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



type alias SpacyToken  =
   { text: String
   , orth: Int
   , lemma_: String
   , pos_: String
   , tag_: String
   , dep_: String
   , shape_: String
   , is_alpha: Bool
   , is_ascii: Bool
   , is_digit: Bool
   , is_punct: Bool
   , is_left_punct: Bool
   , is_right_punct: Bool
   , is_space: Bool
   , is_bracket: Bool
   , is_quote: Bool
   , is_currency: Bool
   , like_url: Bool
   , like_num: Bool
   , like_mail: Bool
   , is_oov: Bool
   , is_stop: Bool
   , head_i: Int
   , left_edge_i: Int
   , right_edge_i: Int
   , i: Int
   , ent_type_: String
   , ent_iob_: String
   , ent_kb_id: Int
   , ent_kb_id_: String
   , norm_: String
   , lang_: String
   , prob: Float
   , idx: Int
   , sentiment: Float
   , lex_id: Int
   , rank: Int
   , cluster: Int
   , is_sent_start: (Maybe Bool)
   }

jsonDecSpacyToken : Json.Decode.Decoder ( SpacyToken )
jsonDecSpacyToken =
   Json.Decode.succeed (\ptext porth plemma_ ppos_ ptag_ pdep_ pshape_ pis_alpha pis_ascii pis_digit pis_punct pis_left_punct pis_right_punct pis_space pis_bracket pis_quote pis_currency plike_url plike_num plike_mail pis_oov pis_stop phead_i pleft_edge_i pright_edge_i pi pent_type_ pent_iob_ pent_kb_id pent_kb_id_ pnorm_ plang_ pprob pidx psentiment plex_id prank pcluster pis_sent_start -> {text = ptext, orth = porth, lemma_ = plemma_, pos_ = ppos_, tag_ = ptag_, dep_ = pdep_, shape_ = pshape_, is_alpha = pis_alpha, is_ascii = pis_ascii, is_digit = pis_digit, is_punct = pis_punct, is_left_punct = pis_left_punct, is_right_punct = pis_right_punct, is_space = pis_space, is_bracket = pis_bracket, is_quote = pis_quote, is_currency = pis_currency, like_url = plike_url, like_num = plike_num, like_mail = plike_mail, is_oov = pis_oov, is_stop = pis_stop, head_i = phead_i, left_edge_i = pleft_edge_i, right_edge_i = pright_edge_i, i = pi, ent_type_ = pent_type_, ent_iob_ = pent_iob_, ent_kb_id = pent_kb_id, ent_kb_id_ = pent_kb_id_, norm_ = pnorm_, lang_ = plang_, prob = pprob, idx = pidx, sentiment = psentiment, lex_id = plex_id, rank = prank, cluster = pcluster, is_sent_start = pis_sent_start})
   |> required "text" (Json.Decode.string)
   |> required "orth" (Json.Decode.int)
   |> required "lemma_" (Json.Decode.string)
   |> required "pos_" (Json.Decode.string)
   |> required "tag_" (Json.Decode.string)
   |> required "dep_" (Json.Decode.string)
   |> required "shape_" (Json.Decode.string)
   |> required "is_alpha" (Json.Decode.bool)
   |> required "is_ascii" (Json.Decode.bool)
   |> required "is_digit" (Json.Decode.bool)
   |> required "is_punct" (Json.Decode.bool)
   |> required "is_left_punct" (Json.Decode.bool)
   |> required "is_right_punct" (Json.Decode.bool)
   |> required "is_space" (Json.Decode.bool)
   |> required "is_bracket" (Json.Decode.bool)
   |> required "is_quote" (Json.Decode.bool)
   |> required "is_currency" (Json.Decode.bool)
   |> required "like_url" (Json.Decode.bool)
   |> required "like_num" (Json.Decode.bool)
   |> required "like_mail" (Json.Decode.bool)
   |> required "is_oov" (Json.Decode.bool)
   |> required "is_stop" (Json.Decode.bool)
   |> required "head_i" (Json.Decode.int)
   |> required "left_edge_i" (Json.Decode.int)
   |> required "right_edge_i" (Json.Decode.int)
   |> required "i" (Json.Decode.int)
   |> required "ent_type_" (Json.Decode.string)
   |> required "ent_iob_" (Json.Decode.string)
   |> required "ent_kb_id" (Json.Decode.int)
   |> required "ent_kb_id_" (Json.Decode.string)
   |> required "norm_" (Json.Decode.string)
   |> required "lang_" (Json.Decode.string)
   |> required "prob" (Json.Decode.float)
   |> required "idx" (Json.Decode.int)
   |> required "sentiment" (Json.Decode.float)
   |> required "lex_id" (Json.Decode.int)
   |> required "rank" (Json.Decode.int)
   |> required "cluster" (Json.Decode.int)
   |> fnullable "is_sent_start" (Json.Decode.bool)

jsonEncSpacyToken : SpacyToken -> Value
jsonEncSpacyToken  val =
   Json.Encode.object
   [ ("text", Json.Encode.string val.text)
   , ("orth", Json.Encode.int val.orth)
   , ("lemma_", Json.Encode.string val.lemma_)
   , ("pos_", Json.Encode.string val.pos_)
   , ("tag_", Json.Encode.string val.tag_)
   , ("dep_", Json.Encode.string val.dep_)
   , ("shape_", Json.Encode.string val.shape_)
   , ("is_alpha", Json.Encode.bool val.is_alpha)
   , ("is_ascii", Json.Encode.bool val.is_ascii)
   , ("is_digit", Json.Encode.bool val.is_digit)
   , ("is_punct", Json.Encode.bool val.is_punct)
   , ("is_left_punct", Json.Encode.bool val.is_left_punct)
   , ("is_right_punct", Json.Encode.bool val.is_right_punct)
   , ("is_space", Json.Encode.bool val.is_space)
   , ("is_bracket", Json.Encode.bool val.is_bracket)
   , ("is_quote", Json.Encode.bool val.is_quote)
   , ("is_currency", Json.Encode.bool val.is_currency)
   , ("like_url", Json.Encode.bool val.like_url)
   , ("like_num", Json.Encode.bool val.like_num)
   , ("like_mail", Json.Encode.bool val.like_mail)
   , ("is_oov", Json.Encode.bool val.is_oov)
   , ("is_stop", Json.Encode.bool val.is_stop)
   , ("head_i", Json.Encode.int val.head_i)
   , ("left_edge_i", Json.Encode.int val.left_edge_i)
   , ("right_edge_i", Json.Encode.int val.right_edge_i)
   , ("i", Json.Encode.int val.i)
   , ("ent_type_", Json.Encode.string val.ent_type_)
   , ("ent_iob_", Json.Encode.string val.ent_iob_)
   , ("ent_kb_id", Json.Encode.int val.ent_kb_id)
   , ("ent_kb_id_", Json.Encode.string val.ent_kb_id_)
   , ("norm_", Json.Encode.string val.norm_)
   , ("lang_", Json.Encode.string val.lang_)
   , ("prob", Json.Encode.float val.prob)
   , ("idx", Json.Encode.int val.idx)
   , ("sentiment", Json.Encode.float val.sentiment)
   , ("lex_id", Json.Encode.int val.lex_id)
   , ("rank", Json.Encode.int val.rank)
   , ("cluster", Json.Encode.int val.cluster)
   , ("is_sent_start", (maybeEncode (Json.Encode.bool)) val.is_sent_start)
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



type alias QueueAddForm  =
   { url: String
   }

jsonDecQueueAddForm : Json.Decode.Decoder ( QueueAddForm )
jsonDecQueueAddForm =
   Json.Decode.succeed (\purl -> {url = purl})
   |> required "url" (Json.Decode.string)

jsonEncQueueAddForm : QueueAddForm -> Value
jsonEncQueueAddForm  val =
   Json.Encode.object
   [ ("url", Json.Encode.string val.url)
   ]



type alias AccountInfo  =
   { id: UserId
   , email: String
   }

jsonDecAccountInfo : Json.Decode.Decoder ( AccountInfo )
jsonDecAccountInfo =
   Json.Decode.succeed (\pid pemail -> {id = pid, email = pemail})
   |> required "id" (jsonDecUserId)
   |> required "email" (Json.Decode.string)

jsonEncAccountInfo : AccountInfo -> Value
jsonEncAccountInfo  val =
   Json.Encode.object
   [ ("id", jsonEncUserId val.id)
   , ("email", Json.Encode.string val.email)
   ]



type alias LogInSendPasswordForm  =
   { email: String
   }

jsonDecLogInSendPasswordForm : Json.Decode.Decoder ( LogInSendPasswordForm )
jsonDecLogInSendPasswordForm =
   Json.Decode.succeed (\pemail -> {email = pemail})
   |> required "email" (Json.Decode.string)

jsonEncLogInSendPasswordForm : LogInSendPasswordForm -> Value
jsonEncLogInSendPasswordForm  val =
   Json.Encode.object
   [ ("email", Json.Encode.string val.email)
   ]



type alias LogInSendCodeForm  =
   { email: String
   , code: String
   }

jsonDecLogInSendCodeForm : Json.Decode.Decoder ( LogInSendCodeForm )
jsonDecLogInSendCodeForm =
   Json.Decode.succeed (\pemail pcode -> {email = pemail, code = pcode})
   |> required "email" (Json.Decode.string)
   |> required "code" (Json.Decode.string)

jsonEncLogInSendCodeForm : LogInSendCodeForm -> Value
jsonEncLogInSendCodeForm  val =
   Json.Encode.object
   [ ("email", Json.Encode.string val.email)
   , ("code", Json.Encode.string val.code)
   ]



type alias QueueItem  =
   { id: QueueId
   , user_id: UserId
   , url: String
   , errored: Bool
   , status: QueueItemStatus
   , article_id: (Maybe ArticleId)
   , created_at: IntZonedTime
   , updated_at: IntZonedTime
   }

jsonDecQueueItem : Json.Decode.Decoder ( QueueItem )
jsonDecQueueItem =
   Json.Decode.succeed (\pid puser_id purl perrored pstatus particle_id pcreated_at pupdated_at -> {id = pid, user_id = puser_id, url = purl, errored = perrored, status = pstatus, article_id = particle_id, created_at = pcreated_at, updated_at = pupdated_at})
   |> required "id" (jsonDecQueueId)
   |> required "user_id" (jsonDecUserId)
   |> required "url" (Json.Decode.string)
   |> required "errored" (Json.Decode.bool)
   |> required "status" (jsonDecQueueItemStatus)
   |> fnullable "article_id" (jsonDecArticleId)
   |> required "created_at" (jsonDecIntZonedTime)
   |> required "updated_at" (jsonDecIntZonedTime)

jsonEncQueueItem : QueueItem -> Value
jsonEncQueueItem  val =
   Json.Encode.object
   [ ("id", jsonEncQueueId val.id)
   , ("user_id", jsonEncUserId val.user_id)
   , ("url", Json.Encode.string val.url)
   , ("errored", Json.Encode.bool val.errored)
   , ("status", jsonEncQueueItemStatus val.status)
   , ("article_id", (maybeEncode (jsonEncArticleId)) val.article_id)
   , ("created_at", jsonEncIntZonedTime val.created_at)
   , ("updated_at", jsonEncIntZonedTime val.updated_at)
   ]



type QueueItemStatus  =
    QueueItemStatusQueued 
    | QueueItemStatusDownloading 
    | QueueItemStatusExtracting 
    | QueueItemStatusNer 
    | QueueItemStatusPos 
    | QueueItemStatusDone 

jsonDecQueueItemStatus : Json.Decode.Decoder ( QueueItemStatus )
jsonDecQueueItemStatus = 
    let jsonDecDictQueueItemStatus = Dict.fromList [("queued", QueueItemStatusQueued), ("downloading", QueueItemStatusDownloading), ("extracting", QueueItemStatusExtracting), ("ner", QueueItemStatusNer), ("pos", QueueItemStatusPos), ("done", QueueItemStatusDone)]
    in  decodeSumUnaries "QueueItemStatus" jsonDecDictQueueItemStatus

jsonEncQueueItemStatus : QueueItemStatus -> Value
jsonEncQueueItemStatus  val =
    case val of
        QueueItemStatusQueued -> Json.Encode.string "queued"
        QueueItemStatusDownloading -> Json.Encode.string "downloading"
        QueueItemStatusExtracting -> Json.Encode.string "extracting"
        QueueItemStatusNer -> Json.Encode.string "ner"
        QueueItemStatusPos -> Json.Encode.string "pos"
        QueueItemStatusDone -> Json.Encode.string "done"


stringEncQueueItemStatus : QueueItemStatus -> String
stringEncQueueItemStatus  val =
    case val of
        QueueItemStatusQueued  -> "queued"
        QueueItemStatusDownloading  -> "downloading"
        QueueItemStatusExtracting  -> "extracting"
        QueueItemStatusNer  -> "ner"
        QueueItemStatusPos  -> "pos"
        QueueItemStatusDone  -> "done"

stringDecQueueItemStatus : String -> Maybe QueueItemStatus
stringDecQueueItemStatus s =
    case s of
        "queued" -> Just QueueItemStatusQueued
        "downloading" -> Just QueueItemStatusDownloading
        "extracting" -> Just QueueItemStatusExtracting
        "ner" -> Just QueueItemStatusNer
        "pos" -> Just QueueItemStatusPos
        "done" -> Just QueueItemStatusDone
        _ -> Nothing

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
type alias ArticlePleaseId = Int
jsonDecArticlePleaseId = Json.Decode.int
jsonEncArticlePleaseId = Json.Encode.int
type alias ArticlePleaseBigId = Int
jsonDecArticlePleaseBigId = Json.Decode.int
jsonEncArticlePleaseBigId = Json.Encode.int
type alias UserId = Int
jsonDecUserId = Json.Decode.int
jsonEncUserId = Json.Encode.int
type alias QueueId = Int
jsonDecQueueId = Json.Decode.int
jsonEncQueueId = Json.Encode.int
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



getApiArticleByArticlepleaseidArticlepleasejson : ArticlePleaseId -> (Result Error  (ArticlePlease)  -> msg) -> Cmd msg
getApiArticleByArticlepleaseidArticlepleasejson capture_article_please_id toMsg =
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
                    , (capture_article_please_id
                       |> String.fromInt)
                    , "article-please.json"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                leExpectJson toMsg jsonDecArticlePlease
            , timeout =
                Nothing
            , tracker =
                Nothing
            }



getApiArticleByArticlepleasebigidArticlepleasebigjson : ArticlePleaseBigId -> (Result Error  (ArticlePleaseBig)  -> msg) -> Cmd msg
getApiArticleByArticlepleasebigidArticlepleasebigjson capture_article_please_big_id toMsg =
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
                    , (capture_article_please_big_id
                       |> String.fromInt)
                    , "article-please-big.json"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                leExpectJson toMsg jsonDecArticlePleaseBig
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



getApiNergroupjson : (Maybe String) -> (Result Error  (NamedEntityGroup)  -> msg) -> Cmd msg
getApiNergroupjson query_ner toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [ [ query_ner
                    |> Maybe.map (Url.Builder.string "ner") ]
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
                    , "ner-group.json"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                leExpectJson toMsg jsonDecNamedEntityGroup
            , timeout =
                Nothing
            , tracker =
                Nothing
            }



postApiQueueAddjson : QueueAddForm -> (Result Error  (())  -> msg) -> Cmd msg
postApiQueueAddjson body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin ""
                    [ "api"
                    , "queue"
                    , "add.json"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncQueueAddForm body)
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



getApiQueuejson : (Result Error  ((List QueueItem))  -> msg) -> Cmd msg
getApiQueuejson toMsg =
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
                    , "queue.json"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                leExpectJson toMsg (Json.Decode.list (jsonDecQueueItem))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }



postApiLoginsendpassword : LogInSendPasswordForm -> (Result Error  (())  -> msg) -> Cmd msg
postApiLoginsendpassword body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin ""
                    [ "api"
                    , "log-in-send-password"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncLogInSendPasswordForm body)
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



postApiLogin : LogInSendCodeForm -> (Result Error  (AccountInfo)  -> msg) -> Cmd msg
postApiLogin body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin ""
                    [ "api"
                    , "log-in"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncLogInSendCodeForm body)
            , expect =
                leExpectJson toMsg jsonDecAccountInfo
            , timeout =
                Nothing
            , tracker =
                Nothing
            }



getApiLogout : (Result Error  (())  -> msg) -> Cmd msg
getApiLogout toMsg =
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
                    , "log-out"
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



getApiAccountinfojson : (Result Error  (AccountInfo)  -> msg) -> Cmd msg
getApiAccountinfojson toMsg =
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
                    , "account-info.json"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                leExpectJson toMsg jsonDecAccountInfo
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

