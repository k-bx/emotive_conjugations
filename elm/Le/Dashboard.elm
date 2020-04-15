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
import Le.Ports
import Le.Routes
import Le.Spacy
import Le.Types exposing (..)
import Le.Utils exposing (..)
import Maybe.Extra
import Process
import SelectTwo
import SelectTwo.Html
import SelectTwo.Types exposing (AjaxParams, SelectTwo, SelectTwoMsg)
import Set
import Task
import Time


type Msg
    = NoOp
    | ToastMsg Le.Block.Toast.Msg
    | UpdateModel Model
    | GotArticles (Result Api.Error (List Api.ArticleShort))
    | CellClicked Api.ArticleId
    | GotArticle (Result Api.Error Api.Article)
    | GotArticlePlease (Result Api.Error Api.ArticlePlease)
    | SelectTwo (SelectTwoMsg Msg)
    | NerSelect String
    | SelectNerAjax AjaxParams Bool
    | GotNers AjaxParams (Result Api.Error (Api.Paginated String))
    | NerClicked Api.CmdSpacyNerResEnt
    | TokenClicked Api.CmdSpacyPosResEnt
    | RenderedSomeTooltipsAndSlept ()


type alias Model =
    { formErrors : Dict String String
    , toasts : Le.Block.Toast.Model
    , key : Browser.Navigation.Key
    , articles : Maybe (List Api.ArticleShort)
    , active : Maybe Api.ArticleId
    , articleFull : Maybe Api.Article
    , articlePlease : Maybe Api.ArticlePlease
    , selectTwo : Maybe (SelectTwo Msg)
    , ners : List String
    , ner : String
    , highlightPos : Bool
    , selectedToken : Maybe Api.CmdSpacyPosResEnt
    , selectedNer : Maybe Api.CmdSpacyNerResEnt
    , tokenCardExpanded : Bool
    }


init : Browser.Navigation.Key -> String -> Maybe Api.ArticleId -> ( Model, Cmd Msg )
init key ner active =
    ( { formErrors = Dict.empty
      , toasts = Le.Block.Toast.init
      , key = key
      , articles = Nothing
      , active = active
      , articleFull = Nothing
      , articlePlease = Nothing
      , selectTwo = Nothing
      , ners = [ ner ]
      , ner = ner
      , highlightPos = False
      , selectedToken = Nothing
      , selectedNer = Nothing
      , tokenCardExpanded = False
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
            , Api.getApiArticleByArticlepleaseidArticlepleasejson article.id GotArticlePlease
            )

        GotArticlePlease (Err e) ->
            handleHttpError ToastMsg e model

        GotArticlePlease (Ok articlePlease) ->
            ( { model | articlePlease = Just articlePlease }
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

        NerClicked ner ->
            ( { model | selectedNer = Just ner }, Cmd.none )

        TokenClicked tok ->
            ( { model | selectedToken = Just tok }
            , Process.sleep 100 |> Task.perform RenderedSomeTooltipsAndSlept
            )

        RenderedSomeTooltipsAndSlept () ->
            ( model, Le.Ports.initTooltips () )


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
                    case model.articlePlease of
                        Nothing ->
                            [ div [] [] ]

                        Just articlePlease ->
                            [ Le.Article.renderContentNodes
                                { nerToHighlight = model.ner
                                , highlightPos = model.highlightPos
                                , onClickToken = TokenClicked
                                , onClickNer = NerClicked
                                , selectedToken = Maybe.map .i model.selectedToken
                                , depChildren = Set.fromList (List.map (\x -> x.i) depChildren)
                                , depParent = Maybe.map .i mDepHeadToken
                                , nodes =
                                    Le.Article.computeContentNodes
                                        { inputText = articlePlease.maintext
                                        , mSpacyNers = articlePlease.spacy_ner_ents
                                        , mSpacyPoss = articlePlease.spacy_pos_ents
                                        }
                                }
                            ]
                ]

        mDepHeadToken : Maybe Api.CmdSpacyPosResEnt
        mDepHeadToken =
            case model.selectedToken of
                Nothing ->
                    Nothing

                Just selectedToken ->
                    model.articlePlease
                        |> Maybe.Extra.unwrap [] (.spacy_pos_ents >> Maybe.withDefault [])
                        |> List.filter (\x -> x.i == selectedToken.head_i)
                        |> List.head

        depChildren : List Api.CmdSpacyPosResEnt
        depChildren =
            case model.selectedToken of
                Nothing ->
                    []

                Just selectedToken ->
                    model.articlePlease
                        |> Maybe.Extra.unwrap [] (.spacy_pos_ents >> Maybe.withDefault [])
                        |> List.filter (\x -> x.head_i == selectedToken.i)

        renderSelectedToken : Api.CmdSpacyPosResEnt -> Html Msg
        renderSelectedToken selectedToken =
            let
                popoverKeyForDep dep =
                    "page-dashboard-popover-dep-" ++ dep

                tooltipHtml dep =
                    "<div>"
                        ++ Le.Spacy.depExplanation dep
                        ++ "</div>"

                -- ++ """
                --    <div class="text-center">
                --      <a href="https://spacy.io/api/annotation#dependency-parsing" target="_blank">see details</a>
                --    </div>
                --    """
                depPopover dep =
                    let
                        key =
                            popoverKeyForDep dep
                    in
                    i
                        [ class "fas fa-question-circle"
                        , id key
                        , attribute "data-toggle" "tooltip"

                        -- , attribute "data-trigger" "click"
                        , attribute "data-html" "true"
                        , attribute "data-original-title" (tooltipHtml dep)

                        -- , attribute "data-offset" "0, 0"
                        ]
                        []

                depTr from dep to =
                    tr []
                        [ td [ class "text-right" ]
                            [ span [ class "badge-highlighed-token badge-highlighed-token--sm badge-highlighed-token--neutral" ]
                                [ text from ]
                            ]
                        , td [ class "applemail__controlpanel__reltable__arrow" ]
                            [ i [ class "fas fa-long-arrow-right" ] [] ]
                        , td [ class "text-center text-nowrap" ]
                            [ span [ class "badge-highlighed-token badge-highlighed-token--sm badge-highlighed-token--neutral" ]
                                [ text <| dep ++ "\u{00A0}"
                                , depPopover dep
                                ]
                            ]

                        -- [ span [ class "badge-highlighed-token badge-highlighed-token--sm badge-highlighed-token--neutral" ]
                        --     [ text <| dep ++ " "
                        --     , depPopover dep
                        --     ]
                        -- ]
                        , td [ class "applemail__controlpanel__reltable__arrow" ]
                            [ i [ class "fas fa-long-arrow-right" ] [] ]
                        , td [ class "text-left" ]
                            [ span [ class "badge-highlighed-token badge-highlighed-token--sm badge-highlighed-token--neutral" ]
                                [ text to ]
                            ]
                        ]

                headTokenRealtionsTrs =
                    case mDepHeadToken of
                        Nothing ->
                            []

                        Just headToken ->
                            [ depTr headToken.text selectedToken.dep_ selectedToken.text
                            ]

                childTokenRelationTr childToken =
                    depTr selectedToken.text childToken.dep_ childToken.text

                relTable =
                    table [ class "applemail__controlpanel__reltable mt-2" ]
                        [ tbody [] <|
                            headTokenRealtionsTrs
                                ++ List.map childTokenRelationTr depChildren
                        ]

                miscDetails =
                    [ div []
                        [ text "Lemma: "
                        , span [] [ text selectedToken.lemma_ ]
                        ]
                    , div []
                        [ text "Part Of Speech: "
                        , span [] [ text selectedToken.pos_ ]
                        ]
                    , div []
                        [ text "Tag: "
                        , span [] [ text selectedToken.tag_ ]
                        ]
                    , div []
                        [ text "Dependency relation: "
                        , span [] [ text selectedToken.dep_ ]
                        ]
                    , div []
                        [ text "Shape: "
                        , span [] [ text selectedToken.shape_ ]
                        ]
                    , div []
                        [ text "is_alpha: "
                        , span [] [ text <| boolYesNo selectedToken.is_alpha ]
                        ]
                    , div []
                        [ text "is_ascii: "
                        , span [] [ text <| boolYesNo selectedToken.is_ascii ]
                        ]
                    , div []
                        [ text "is_digit: "
                        , span [] [ text <| boolYesNo selectedToken.is_digit ]
                        ]
                    , div []
                        [ text "is_punct: "
                        , span [] [ text <| boolYesNo selectedToken.is_punct ]
                        ]
                    , div []
                        [ text "is_left_punct: "
                        , span [] [ text <| boolYesNo selectedToken.is_left_punct ]
                        ]
                    , div []
                        [ text "is_right_punct: "
                        , span [] [ text <| boolYesNo selectedToken.is_right_punct ]
                        ]
                    , div []
                        [ text "is_space: "
                        , span [] [ text <| boolYesNo selectedToken.is_space ]
                        ]
                    , div []
                        [ text "is_bracket: "
                        , span [] [ text <| boolYesNo selectedToken.is_bracket ]
                        ]
                    , div []
                        [ text "is_quote: "
                        , span [] [ text <| boolYesNo selectedToken.is_quote ]
                        ]
                    , div []
                        [ text "is_currency: "
                        , span [] [ text <| boolYesNo selectedToken.is_currency ]
                        ]
                    , div []
                        [ text "like_url: "
                        , span [] [ text <| boolYesNo selectedToken.like_url ]
                        ]
                    , div []
                        [ text "like_num: "
                        , span [] [ text <| boolYesNo selectedToken.like_num ]
                        ]
                    , div []
                        [ text "like_mail: "
                        , span [] [ text <| boolYesNo selectedToken.like_mail ]
                        ]
                    , div []
                        [ text "is_oov: "
                        , span [] [ text <| boolYesNo selectedToken.is_oov ]
                        ]
                    , div []
                        [ text "is_stop: "
                        , span [] [ text <| boolYesNo selectedToken.is_stop ]
                        ]
                    , div []
                        [ text "head_i: "
                        , span [] [ text <| String.fromInt selectedToken.head_i ]
                        ]
                    , div []
                        [ text "left_edge_i: "
                        , span [] [ text <| String.fromInt selectedToken.left_edge_i ]
                        ]
                    , div []
                        [ text "right_edge_i: "
                        , span [] [ text <| String.fromInt selectedToken.right_edge_i ]
                        ]
                    , div []
                        [ text "i: "
                        , span [] [ text <| String.fromInt selectedToken.i ]
                        ]
                    , div []
                        [ text "ent_type_: "
                        , span [] [ text <| selectedToken.ent_type_ ]
                        ]
                    , div []
                        [ text "ent_iob_: "
                        , span [] [ text <| selectedToken.ent_iob_ ]
                        ]
                    , div []
                        [ text "ent_kb_id: "
                        , span [] [ text <| String.fromInt selectedToken.ent_kb_id ]
                        ]
                    , div []
                        [ text "ent_kb_id_: "
                        , span [] [ text <| selectedToken.ent_kb_id_ ]
                        ]
                    , div []
                        [ text "norm_: "
                        , span [] [ text <| selectedToken.norm_ ]
                        ]
                    , div []
                        [ text "lang_: "
                        , span [] [ text <| selectedToken.lang_ ]
                        ]
                    , div []
                        [ text "prob: "
                        , span [] [ text <| String.fromFloat selectedToken.prob ]
                        ]
                    , div []
                        [ text "idx: "
                        , span [] [ text <| String.fromInt selectedToken.idx ]
                        ]
                    , div []
                        [ text "sentiment: "
                        , span [] [ text <| String.fromFloat selectedToken.sentiment ]
                        ]
                    , div []
                        [ text "lex_id: "
                        , span [] [ text <| String.fromInt selectedToken.lex_id ]
                        ]
                    , div []
                        [ text "rank: "
                        , span [] [ text <| String.fromInt selectedToken.rank ]
                        ]
                    , div []
                        [ text "cluster: "
                        , span [] [ text <| String.fromInt selectedToken.cluster ]
                        ]
                    ]
            in
            div
                [ class "mt-2"
                , class "details-board fade-in"
                , classList [ ( "details-board--collapsed", not model.tokenCardExpanded ) ]
                ]
            <|
                [ div [ class "details-board__content" ] <|
                    [ div [ class "text-center mb-4" ]
                        [ span [ class "badge-highlighed-token" ] [ text selectedToken.text ]
                        ]
                    ]
                        ++ [ div [ class "d-flex justify-content-center" ] [ relTable ] ]
                        ++ [ div [ class "details-board__row" ] miscDetails ]
                , div [ class "details-board__toggle" ]
                    [ span
                        [ class "details-board__toggle__switch"
                        , onClick <| UpdateModel { model | tokenCardExpanded = not model.tokenCardExpanded }
                        ]
                        [ text <|
                            case model.tokenCardExpanded of
                                True ->
                                    "show less"

                                False ->
                                    "show more"
                        ]
                    ]
                ]

        renderSelectedNer : Api.CmdSpacyNerResEnt -> Html Msg
        renderSelectedNer selectedNer =
            div
                [ class "mt-2"
                , class "details-board fade-in"
                ]
                [ div [ class "text-center mb-4" ]
                    [ span [ class "badge-highlighed-token badge-highlighed-token--ner" ] [ text selectedNer.text ]
                    ]
                ]

        articleControlPanel =
            div [] <|
                [ div [ class "mt-2 d-flex flex-row justify-content-between" ]
                    [ text "Highlight POS"
                    , div [ class "switch" ]
                        [ input
                            [ class "switch-input"
                            , id "highlight-pos-checkbox"
                            , name "check"
                            , type_ "checkbox"
                            , checked model.highlightPos
                            , onClick (UpdateModel { model | highlightPos = not model.highlightPos })
                            ]
                            []
                        , label
                            [ class "switch-label mb-0"
                            , for "highlight-pos-checkbox"
                            ]
                            [ text "switch" ]
                        ]
                    ]
                ]
                    ++ (model.selectedToken
                            |> Maybe.map renderSelectedToken
                            |> Maybe.Extra.unwrap [] List.singleton
                       )
                    ++ (model.selectedNer
                            |> Maybe.map renderSelectedNer
                            |> Maybe.Extra.unwrap [] List.singleton
                       )
    in
    main_ [ class "main-content", attribute "role" "main" ]
        [ div [ class "applemail" ]
            [ div [ class "applemail__articles" ] <|
                [ articlesNav ]
            , div [ class "applemail__content" ]
                [ articleFullDetailsBlock
                ]
            , div [ class "applemail__controlpanel" ]
                [ articleControlPanel
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
