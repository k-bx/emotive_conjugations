module Le.Routes exposing (..)

import Le.Api as Api
import Le.Config exposing (..)
import Le.Utils exposing (..)
import Time
import Url.Builder


dashboard : String -> Maybe Api.ArticleId -> String
dashboard ner mArticleId =
    case ( ner, mArticleId ) of
        ( "", Nothing ) ->
            "/dashboard"

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
