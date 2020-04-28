module Le.Routes exposing (..)

import Le.Api as Api
import Le.Config exposing (..)
import Le.Utils exposing (..)
import Time
import Url.Builder


index =
    Url.Builder.absolute [ "" ] []


dashboard : String -> Maybe Api.ArticleId -> String
dashboard ner mArticleId =
    case ( ner, mArticleId ) of
        ( "", Nothing ) ->
            Url.Builder.absolute [ "dashboard" ] []

        ( _, Nothing ) ->
            Url.Builder.absolute
                [ "dashboard" ]
                [ Url.Builder.string "ner" ner ]

        ( _, Just articleId ) ->
            Url.Builder.absolute
                [ "dashboard" ]
                [ Url.Builder.string "ner" ner
                , Url.Builder.int "article-id" articleId
                ]


queue : String
queue =
    Url.Builder.absolute [ "queue" ] []


login : String
login =
    Url.Builder.absolute [ "login" ] []


logout : String
logout =
    Url.Builder.absolute [ "api", "log-out" ] []
