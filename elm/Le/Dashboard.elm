module Le.Dashboard exposing (..)

import Browser.Navigation
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
import SelectTwo
import SelectTwo.Html
import SelectTwo.Types exposing (AjaxParams, SelectTwo, SelectTwoMsg)
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
    | SelectTwo (SelectTwoMsg Msg)
    | NerSelect String
    | SelectNerAjax AjaxParams Bool
    | GotNers AjaxParams (Result Api.Error (Api.Paginated String))


type alias Model =
    { formErrors : Dict String String
    , toasts : Le.Block.Toast.Model
    , key : Browser.Navigation.Key
    , articles : Maybe (List Api.ArticleShort)
    , active : Maybe Api.ArticleId
    , articleFull : Maybe Api.Article
    , articleNp : Maybe Api.ArticleNp
    , selectTwo : Maybe (SelectTwo Msg)
    , ners : List String
    , ner : String
    }


init : Browser.Navigation.Key -> String -> Maybe Api.ArticleId -> ( Model, Cmd Msg )
init key ner active =
    ( { formErrors = Dict.empty
      , toasts = Le.Block.Toast.init
      , key = key
      , articles = Nothing
      , active = active
      , articleFull = Nothing
      , articleNp = Nothing
      , selectTwo = Nothing
      , ners = [ ner ]
      , ner = ner
      }
    , Cmd.batch <|
        [ Api.getApiArticlesshortjson (Just ner) GotArticles
        ]
            ++ (case active of
                    Nothing ->
                        [ Cmd.none ]

                    Just articleId ->
                        [ Api.getApiArticleByArticleidArticlejson articleId GotArticle ]
               )
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
            let
                m2 =
                    { model | active = Just articleId }
            in
            ( m2
            , Cmd.batch
                [ Api.getApiArticleByArticleidArticlejson articleId GotArticle
                , Browser.Navigation.replaceUrl model.key <|
                    Le.Routes.dashboard m2.ner m2.active
                ]
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

        SelectTwo stmsg ->
            let
                ajaxCases =
                    Just
                        (\id_ params reset ->
                            case id_ of
                                "select-ner" ->
                                    ( model
                                    , SelectTwo.send <| SelectNerAjax params reset
                                    )

                                _ ->
                                    ( model
                                    , Cmd.none
                                    )
                        )
            in
            SelectTwo.update SelectTwo stmsg ajaxCases model

        SelectNerAjax params reset ->
            let
                m2 =
                    SelectTwo.setLoading params reset model

                m3 =
                    { m2 | articles = Nothing }
            in
            ( m3
            , Api.getApiPersonnamedentitieslistjson (Just params.term) (Just params.page) (GotNers params)
            )

        NerSelect ner ->
            let
                m2 =
                    { model | ner = ner, articles = Nothing }
            in
            ( m2
            , Cmd.batch
                [ Api.getApiArticlesshortjson (Just ner) GotArticles
                , Browser.Navigation.replaceUrl model.key <|
                    Le.Routes.dashboard m2.ner m2.active
                ]
            )

        GotNers params (Err e) ->
            handleHttpError ToastMsg e model

        GotNers params (Ok ners) ->
            let
                m2 =
                    { model | ners = ners.items }

                list : List (SelectTwo.Types.GroupSelectTwoOption Msg)
                list =
                    SelectTwo.basicSelectOptions NerSelect (List.map (\x -> ( x, x )) ners.items)

                newParams : AjaxParams
                newParams =
                    { params | more = params.page < ners.overall_pages }
            in
            ( SelectTwo.setList list newParams m2
            , Cmd.none
            )


navbarContent : Html Msg
navbarContent =
    header []
        [ nav [ class "navbar navbar-expand-md navbar-dark bg-dark" ]
            [ a
                [ class "navbar-brand"
                , href <| Le.Routes.dashboard "" Nothing
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
                            , href <| Le.Routes.dashboard "" Nothing
                            ]
                            [ text "Dashboard"
                            ]
                        ]
                    ]
                ]
            ]
        ]


searchPanel : Model -> Html Msg
searchPanel model =
    div [ class "search-panel" ]
        [ div [ class "ml-2" ]
            [ let
                opts =
                    SelectTwo.basicSelectOptions NerSelect
                        (model.ners
                            |> List.map (\ner -> ( ner, ner ))
                        )
              in
              SelectTwo.Html.select2 SelectTwo
                { defaults =
                    SelectTwo.defaultsFromList [ NerSelect model.ner ] opts
                , ajax = True
                , delay = 300
                , id_ = "select-ner"
                , clearMsg = Nothing
                , showSearch = True
                , width = "300px"
                , placeholder = "Select Named Entity"
                , list = opts
                , multiSelect = False
                , disabled = False
                , noResultsMessage = Just "No results"
                , closeOnClear = False
                }
            ]
        ]


mainContent : Model -> Html Msg
mainContent model =
    let
        articlesNav =
            div [ class "articles-nav" ] <|
                case model.articles of
                    Nothing ->
                        [ div [ class "text-center mt-4" ] [ loadingSpinner ] ]

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
                        [ text
                            (article.date
                                |> Maybe.map (renderDateTimeline << Time.millisToPosix)
                                |> Maybe.withDefault "--"
                            )
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
                            [ Le.Article.renderContent
                                { nerToHighlight = model.ner
                                , inputText = articleNp.content
                                , mSpacyNers = articleNp.spacy_ner_ents
                                , mSpacyPoss = articleNp.spacy_pos_ents
                                }
                            ]
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
        div
            [ class "h-100 gr__getbootstrap_com page-dashboard"
            , SelectTwo.Html.select2Close SelectTwo
            ]
            [ div [ class "d-flex flex-column h-100" ]
                [ navbarContent
                , searchPanel model
                , mainContent model
                , footerContent
                ]
            , SelectTwo.Html.select2Dropdown SelectTwo Nothing model
            ]
