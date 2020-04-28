module Le.Pages.Dashboard exposing (..)

import Browser.Navigation
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Le.Api as Api
import Le.Article
import Le.Block.Dashboard
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
    | GotArticlePleaseBig (Result Api.Error Api.ArticlePleaseBig)
    | SelectTwo (SelectTwoMsg Msg)
    | NerSelect String
    | SelectNerAjax AjaxParams Bool
    | GotNers AjaxParams (Result Api.Error (Api.Paginated String))
    | NerClicked Api.CmdSpacyNerResEnt
    | TokenClicked Api.CmdSpacyPosResEnt
    | RenderedSomeTooltipsAndSlept ()
    | GotNerGroup (Result Api.Error Api.NamedEntityGroup)


type alias Model =
    { formErrors : Dict String String
    , toasts : Le.Block.Toast.Model
    , key : Browser.Navigation.Key
    , articles : Maybe (List Api.ArticleShort)
    , active : Maybe Api.ArticleId
    , articleFull : Maybe Api.Article
    , articlePlease : Maybe Api.ArticlePlease
    , articlePleaseBig : Maybe Api.ArticlePleaseBig
    , selectTwo : Maybe (SelectTwo Msg)
    , ners : List String
    , ner : String
    , highlightPos : Bool
    , highlightAllNers : Bool
    , selectedToken : Maybe Api.CmdSpacyPosResEnt
    , selectedNer : Maybe Api.CmdSpacyNerResEnt
    , tokenCardExpanded : Bool
    , nerGroup : Maybe Api.NamedEntityGroup
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
      , articlePleaseBig = Nothing
      , selectTwo = Nothing
      , ners = [ ner ]
      , ner = ner
      , highlightPos = False
      , highlightAllNers = False
      , selectedToken = Nothing
      , selectedNer = Nothing
      , tokenCardExpanded = False
      , nerGroup = Nothing
      }
    , Cmd.batch <|
        [ Api.getApiArticlesshortjson (Just ner) GotArticles
        , Api.getApiNergroupjson (Just ner) GotNerGroup
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
            , Cmd.batch
                [ Api.getApiArticleByArticlepleaseidArticlepleasejson article.id GotArticlePlease
                , Api.getApiArticleByArticlepleasebigidArticlepleasebigjson article.id GotArticlePleaseBig
                ]
            )

        GotArticlePlease (Err e) ->
            handleHttpError ToastMsg e model

        GotArticlePlease (Ok articlePlease) ->
            ( { model | articlePlease = Just articlePlease }
            , Cmd.none
            )

        GotArticlePleaseBig (Err e) ->
            handleHttpError ToastMsg e model

        GotArticlePleaseBig (Ok articlePleaseBig) ->
            ( { model | articlePleaseBig = Just articlePleaseBig }
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
                , Api.getApiNergroupjson (Just ner) GotNerGroup
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
            ( { model | selectedNer = Just ner }
            , Cmd.none
            )

        TokenClicked tok ->
            ( { model | selectedToken = Just tok }
            , Process.sleep 100 |> Task.perform RenderedSomeTooltipsAndSlept
            )

        RenderedSomeTooltipsAndSlept () ->
            ( model, Le.Ports.initTooltips () )

        GotNerGroup (Err e) ->
            handleHttpError ToastMsg e model

        GotNerGroup (Ok nerGroup) ->
            ( { model | nerGroup = Just nerGroup }
            , Cmd.none
            )


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

        articleDetails : Api.Article -> Html Msg
        articleDetails article =
            let
                articlePleaseBadges =
                    case model.articlePlease of
                        Nothing ->
                            []

                        Just articlePlease ->
                            [ span [ class "badge badge-info" ]
                                [ text <|
                                    "date-download:"
                                        ++ (articlePlease.date_download
                                                |> Maybe.map (renderDateInfobox << Time.millisToPosix)
                                                |> Maybe.withDefault "none"
                                           )
                                ]
                            , text " "
                            , span [ class "badge badge-info" ]
                                [ text <|
                                    "date-publish:"
                                        ++ (articlePlease.date_publish
                                                |> Maybe.map (renderDateInfobox << Time.millisToPosix)
                                                |> Maybe.withDefault "none"
                                           )
                                ]
                            , text " "
                            , span [ class "badge badge-info" ]
                                [ text <|
                                    "date-modify:"
                                        ++ (articlePlease.date_modify
                                                |> Maybe.map (renderDateInfobox << Time.millisToPosix)
                                                |> Maybe.withDefault "none"
                                           )
                                ]
                            ]
            in
            div []
                [ div [ class "details-board__content__infobox" ]
                    [ div [ class "details-board__content__infobox__date" ]
                        [ text
                            (article.date
                                |> Maybe.map (renderDateInfobox << Time.millisToPosix)
                                |> Maybe.withDefault "--"
                            )
                        ]
                    , div [] [ strong [] [ text article.paper_name ] ]
                    , div [] <|
                        [ span [ class "badge badge-info" ]
                            [ text <| "id:" ++ String.fromInt article.id ]
                        , text " "
                        , span [ class "badge badge-info" ]
                            [ text <| "warc-id:" ++ Maybe.withDefault "<unknown>" article.warc_id ]
                        ]
                    , div [] <|
                        articlePleaseBadges
                    , div []
                        [ a
                            [ href article.url
                            , target "_blank"
                            ]
                            [ text <| String.left 300 article.url ]
                        ]
                    ]
                , div [ class "details-board__content__article" ]
                    [ h2 [] [ text article.title ]
                    , div [ class "article" ] <|
                        case ( model.articlePlease, model.articlePleaseBig ) of
                            ( Just articlePlease, Just articlePleaseBig ) ->
                                [ Le.Article.renderContentNodes
                                    { nersToHighlight = model.nerGroup |> Maybe.Extra.unwrap [] .group
                                    , highlightPos = model.highlightPos
                                    , highlightAllNers = model.highlightAllNers
                                    , onClickToken = TokenClicked
                                    , onClickNer = NerClicked
                                    , selectedToken = Maybe.map .i model.selectedToken
                                    , depChildren = Set.fromList (List.map (\x -> x.i) depChildren)
                                    , depParent = Maybe.map .i mDepHeadToken
                                    , nodes =
                                        Le.Article.computeContentNodes
                                            { inputText = articlePleaseBig.maintext
                                            , mSpacyNers = articlePleaseBig.spacy_ner_ents
                                            , mSpacyPoss = articlePleaseBig.spacy_pos_ents
                                            }
                                    }
                                ]

                            _ ->
                                [ div [] [] ]
                    ]
                ]

        mDepHeadToken : Maybe Api.CmdSpacyPosResEnt
        mDepHeadToken =
            case model.selectedToken of
                Nothing ->
                    Nothing

                Just selectedToken ->
                    model.articlePleaseBig
                        |> Maybe.Extra.unwrap [] (.spacy_pos_ents >> Maybe.withDefault [])
                        |> List.filter (\x -> x.i == selectedToken.head_i)
                        |> List.head

        depChildren : List Api.CmdSpacyPosResEnt
        depChildren =
            case model.selectedToken of
                Nothing ->
                    []

                Just selectedToken ->
                    model.articlePleaseBig
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
                    [ span [ class "badge-highlighed-token badge-highlighed-token--ner" ] [ text selectedNer.text ] ]
                , div [ class "details-board__row" ]
                    [ div []
                        [ text "label_: "
                        , span [] [ text <| selectedNer.label_ ]
                        ]
                    ]
                ]

        renderNerGroup nerGroup =
            let
                renderItem t =
                    li [ class "mb-2" ]
                        [ span [ class "badge-highlighed-token badge-highlighed-token--ner" ]
                            [ text t ]
                        ]
            in
            div [ class "mt-2 details-board fade-in" ] <|
                [ div [ class "text-center mb-4" ]
                    [ -- span [ class "badge-highlighed-token badge-highlighed-token--ner" ]
                      --    [ text <| nerGroup.entity
                      --    ]
                      span [ class "badge-highlighed-nearby-font badge-highlighed-nearby-font--white" ] [ text " NER group" ]
                    ]
                , ul [] <|
                    List.map renderItem nerGroup.group
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
                , div [ class "mt-2 d-flex flex-row justify-content-between" ]
                    [ text "Highlight all NERs"
                    , div [ class "switch" ]
                        [ input
                            [ class "switch-input"
                            , id "highlight-all-ners-checkbox"
                            , name "check"
                            , type_ "checkbox"
                            , checked model.highlightAllNers
                            , onClick (UpdateModel { model | highlightAllNers = not model.highlightAllNers })
                            ]
                            []
                        , label
                            [ class "switch-label mb-0"
                            , for "highlight-all-ners-checkbox"
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
                    ++ (model.nerGroup
                            |> Maybe.map renderNerGroup
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


view : ViewParams -> Model -> Html Msg
view vps model =
    Le.Block.Toast.view ToastMsg vps.now model.toasts <|
        div
            [ class "h-100 gr__getbootstrap_com page-dashboard"
            , SelectTwo.Html.select2Close SelectTwo
            ]
            [ Le.Block.Dashboard.view vps.routeName <|
                div []
                    [ searchPanel model
                    , mainContent model
                    ]
            , SelectTwo.Html.select2Dropdown SelectTwo Nothing model
            ]
