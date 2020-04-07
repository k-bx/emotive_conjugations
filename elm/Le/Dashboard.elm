module Le.Dashboard exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Le.Api as Api
import Le.Article
import Le.Block.Toast
import Le.Components exposing (..)
import Le.Config
import Le.Lib exposing (..)
import Le.Routes
import Le.Types exposing (..)
import Le.Utils exposing (..)
import Maybe.Extra
import Process
import Task
import Time


type Msg
    = NoOp
    | ToastMsg Le.Block.Toast.Msg
    | UpdateModel Model
    | GotArticles (Result Api.Error (List Api.ArticleShort))
    | CellClicked Api.ArticleId
    | GotArticle (Result Api.Error Api.Article)
    | GotArticleNp (Result Api.Error Api.ArticleNp)


type alias Model =
    { formErrors : Dict String String
    , toasts : Le.Block.Toast.Model
    , articles : Maybe (List Api.ArticleShort)
    , active : Maybe Api.ArticleId
    , articleFull : Maybe Api.Article
    , articleNp : Maybe Api.ArticleNp
    }


init : ( Model, Cmd Msg )
init =
    ( { formErrors = Dict.empty
      , toasts = Le.Block.Toast.init
      , articles = Nothing
      , active = Nothing
      , articleFull = Nothing
      , articleNp = Nothing
      }
    , Api.getApiArticlesshortjson GotArticles
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

        GotArticles (Err e) ->
            handleHttpError ToastMsg e model

        GotArticles (Ok articles) ->
            ( { model | articles = Just articles }
            , Cmd.none
            )

        CellClicked articleId ->
            ( { model | active = Just articleId }
            , Api.getApiArticleByArticleidArticlejson articleId GotArticle
            )

        GotArticle (Err e) ->
            handleHttpError ToastMsg e model

        GotArticle (Ok article) ->
            ( { model | articleFull = Just article }
            , Api.getApiArticleByArticlenpidArticlenpjson article.id GotArticleNp
            )

        GotArticleNp (Err e) ->
            handleHttpError ToastMsg e model

        GotArticleNp (Ok articleNp) ->
            ( { model | articleNp = Just articleNp }
            , Cmd.none
            )


navbarContent : Html Msg
navbarContent =
    header []
        [ nav [ class "navbar navbar-expand-md navbar-dark bg-dark" ]
            [ a
                [ class "navbar-brand"
                , href <| Le.Routes.dashboard
                ]
                [ text "Emotive Conjugations" ]
            , button [ attribute "aria-controls" "navbarCollapse", attribute "aria-expanded" "false", attribute "aria-label" "Toggle navigation", class "navbar-toggler", attribute "data-target" "#navbarCollapse", attribute "data-toggle" "collapse", type_ "button" ]
                [ span [ class "navbar-toggler-icon" ]
                    []
                ]
            , div [ class "collapse navbar-collapse", id "navbarCollapse" ]
                [ ul [ class "navbar-nav mr-auto" ]
                    [ li [ class "nav-item active" ]
                        [ a
                            [ class "nav-link"
                            , href <| Le.Routes.dashboard
                            ]
                            [ text "Dashboard"
                            ]
                        ]
                    ]
                ]
            ]
        ]


mainContent : Model -> Html Msg
mainContent model =
    let
        articlesNav =
            div [ class "articles-nav" ] <|
                case model.articles of
                    Nothing ->
                        [ div [ class "text-center" ] [ loadingSpinner ] ]

                    Just articles ->
                        List.map renderArticleNav articles

        renderArticleNav : Api.ArticleShort -> Html Msg
        renderArticleNav article =
            div
                [ class "articles-nav__cell"
                , classList [ ( "articles-nav__cell--active", Just article.id == model.active ) ]
                , onClick <| CellClicked article.id
                ]
                [ div [ class "articles-nav__cell__date" ]
                    [ div []
                        [ text <| renderDateTimeline (Time.millisToPosix article.date)
                        ]
                    ]
                , div [ class "" ]
                    [ div [ class "articles-nav__cell__maintitle" ]
                        [ strong [] [ text article.paper_name ]
                        ]
                    , div [ class "articles-nav__cell__secondary" ]
                        [ text article.title_short
                        ]
                    ]
                ]

        articleFullDetailsBlock =
            case model.articleFull of
                Nothing ->
                    div [] []

                Just article ->
                    articleDetails article

        articleDetails article =
            div []
                [ h2 [] [ text article.title ]
                , div [ class "article" ] <|
                    case model.articleNp of
                        Nothing ->
                            [ div [] [] ]

                        Just articleNp ->
                            Le.Article.renderContent articleNp.content articleNp.spacy_ner_ents
                ]
    in
    main_ [ class "main-content", attribute "role" "main" ]
        [ div [ class "applemail" ]
            [ div [ class "applemail__articles" ] <|
                [ articlesNav ]
            , div [ class "applemail__content" ]
                [ articleFullDetailsBlock
                ]
            ]
        ]


footerContent : Html Msg
footerContent =
    footer [ class "footer mt-auto py-3" ]
        [ div [ class "text-center" ]
            [ span [ class "text-muted" ]
                [ text "Made by "
                , a [ href "https://k-bx.github.io", target "_blank" ] [ text "Konstantine Rybnikov" ]
                , text " with help from "
                , a [ href "https://theportal.wiki/", target "_blank" ] [ text "The Portal" ]
                , text " community"
                ]
            ]
        ]


view : ViewParams -> Model -> Html Msg
view vps model =
    Le.Block.Toast.view ToastMsg vps.now model.toasts <|
        div [ class "h-100 gr__getbootstrap_com page-dashboard" ]
            [ div [ class "d-flex flex-column h-100" ]
                [ navbarContent
                , mainContent model
                , footerContent
                ]
            ]
