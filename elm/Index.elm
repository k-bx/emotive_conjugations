module Index exposing (main)

import Browser exposing (UrlRequest)
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Le.Api as Api
import Le.Dashboard as Dashboard
import Le.Types exposing (..)
import Le.Utils exposing (..)
import Task
import Time
import Url exposing (Url)
import Url.Parser as P exposing ((</>), (<?>), Parser, int, map, oneOf, s, string)
import Url.Parser.Query as Q


type alias Flags =
    {}


type Msg
    = NoOp
    | GotTime Time.Posix
    | GotViewport Browser.Dom.Viewport
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | ResizeHappened Int Int
    | DashboardMsg Dashboard.Msg


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , route : Route
    , pmodel : PageModel
    , viewport : Maybe Browser.Dom.Viewport
    , now : Maybe Time.Posix
    , errors : List String
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( dashboardInitModel, dashboardInitCmds ) =
            Dashboard.init key "" Nothing

        m : Model
        m =
            { key = key
            , url = url
            , route = PageNotFound
            , pmodel = DashboardModel dashboardInitModel
            , viewport = Nothing
            , now = Nothing
            , errors = []
            }

        ( m2, cmds ) =
            onUrlChanged m url

        getViewportCmd =
            Task.perform GotViewport Browser.Dom.getViewport

        cmds2 =
            Cmd.batch
                [ cmds
                , getViewportCmd
                , Task.perform GotTime Time.now
                ]
    in
    ( m2, cmds2 )


type PageModel
    = DashboardModel Dashboard.Model
    | PageNotFoundModel ()


type Route
    = Dashboard String (Maybe Api.ArticleId)
    | PageNotFound


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ P.map (Dashboard "" Nothing) P.top
        , P.map
            (\( mn, a ) -> Dashboard (Maybe.withDefault "" mn) a)
            (P.s "dashboard" <?> Q.map2 (\a b -> ( a, b )) (Q.string "ner") (Q.int "article-id"))
        ]


onUrlChanged : Model -> Url -> ( Model, Cmd Msg )
onUrlChanged model url =
    case P.parse routeParser url of
        Just route ->
            case sameTopRoute route model.route of
                True ->
                    ( { model | route = route, url = url }, Cmd.none )

                False ->
                    let
                        ( pm, cmds ) =
                            initPageModel model.key model.url route
                    in
                    ( { model | route = route, url = url, pmodel = pm }, cmds )

        Nothing ->
            ( { model
                | url = url
                , route = PageNotFound
              }
            , Cmd.none
            )


initSubpage doInit toModel toMsg =
    let
        ( m, cmds ) =
            doInit
    in
    ( toModel m, Cmd.map toMsg cmds )


initPageModel : Nav.Key -> Url.Url -> Route -> ( PageModel, Cmd Msg )
initPageModel key url r =
    case r of
        Dashboard ner article ->
            initSubpage (Dashboard.init key ner article) DashboardModel DashboardMsg

        PageNotFound ->
            ( PageNotFoundModel (), Cmd.none )


sameTopRoute : Route -> Route -> Bool
sameTopRoute r1 r2 =
    case ( r1, r2 ) of
        ( Dashboard _ _, Dashboard _ _ ) ->
            True

        _ ->
            False


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize ResizeHappened

        -- , Time.every 5000 GotTime
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        setpm c pm =
            { model | pmodel = c pm }
    in
    case ( model.pmodel, msg ) of
        ( _, NoOp ) ->
            ( model, Cmd.none )

        ( _, GotTime t ) ->
            ( { model | now = Just t }, Cmd.none )

        ( _, GotViewport vp ) ->
            ( { model | viewport = Just vp }, Cmd.none )

        ( _, LinkClicked urlRequest ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( { model | url = url }
                    , Cmd.batch
                        [ Nav.pushUrl model.key (Url.toString url)
                        ]
                    )

                Browser.External href ->
                    ( model, Nav.load href )

        ( _, UrlChanged url ) ->
            onUrlChanged model url

        ( _, ResizeHappened _ _ ) ->
            ( model, Task.perform GotViewport Browser.Dom.getViewport )

        ( DashboardModel imodel, DashboardMsg imsg ) ->
            subpage model imodel imsg (setpm DashboardModel) Dashboard.update DashboardMsg Dashboard

        ( PageNotFoundModel imodel, _ ) ->
            ( { model | route = PageNotFound, pmodel = PageNotFoundModel () }, Cmd.none )



-- ( _, _ ) ->
--     ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Emotive Conjugations"
    , body = [ mainContent model ]
    }


mainContent : Model -> Html Msg
mainContent model =
    let
        isMob =
            case model.viewport of
                Nothing ->
                    False

                Just v ->
                    isMobile v

        viewParams =
            { isMobile = isMob
            , now = model.now
            }
    in
    case ( model.route, model.pmodel ) of
        ( Dashboard ner article, DashboardModel pmodel ) ->
            Html.map DashboardMsg (Dashboard.view viewParams pmodel)

        ( _, _ ) ->
            pageNotFound


pageNotFound =
    text "page not found"


subpage model imodel imsg setPageModel pageUpdate toMsg page =
    let
        ( newPageModel, pageCmd ) =
            pageUpdate imsg imodel

        newModel =
            setPageModel newPageModel

        newCmd =
            Cmd.map toMsg pageCmd
    in
    ( newModel, newCmd )
