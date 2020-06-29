module Le.Article exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Le.Api as Api
import List.Extra
import Maybe.Extra
import Set exposing (Set)


type ContentNode
    = CNText
        { begin : Int
        , end : Int
        , text : String
        }
    | CNParagraph
        { begin : Int
        , end : Int
        , children : List ContentNode
        }
    | CNNer
        { begin : Int
        , end : Int
        , ner : Api.CmdSpacyNerResEnt
        , children : List ContentNode
        }
    | CNTok
        { begin : Int
        , end : Int
        , tok : Api.SpacyToken
        , children : List ContentNode
        }


computeContentNodes :
    { inputText : String
    , mSpacyNers : Maybe (List Api.CmdSpacyNerResEnt)
    , mSpacyPoss : Maybe (List Api.SpacyToken)
    }
    -> List ContentNode
computeContentNodes ps =
    let
        parDelim =
            "\n"

        parDelimLen =
            String.length parDelim

        parEndsDots =
            String.indexes parDelim (ps.inputText ++ parDelim)

        paragraphs : List ContentNode
        paragraphs =
            List.Extra.zip ([ -1 * parDelimLen ] ++ parEndsDots) parEndsDots
                |> List.map
                    (\( prevParEnd, parEnd ) ->
                        let
                            begin =
                                prevParEnd + parDelimLen

                            end =
                                parEnd
                        in
                        CNParagraph
                            { begin = begin
                            , end = end
                            , children = computeNersAndToks begin (String.slice begin end ps.inputText)
                            }
                    )

        computeNersAndToks : Int -> String -> List ContentNode
        computeNersAndToks parBeginAbs parText =
            let
                foldFunc ner acc =
                    if ner.start_char < acc.parTextLeftPosAbs then
                        acc

                    else if ner.start_char >= acc.parTextLeftPosAbs + String.length acc.parTextLeft then
                        acc

                    else
                        let
                            textBeforeNer =
                                String.left (ner.start_char - acc.parTextLeftPosAbs) acc.parTextLeft

                            textFromNer =
                                String.dropLeft (ner.start_char - acc.parTextLeftPosAbs) acc.parTextLeft

                            textNer =
                                String.left (String.length ner.text) textFromNer

                            textAfterNer =
                                String.dropLeft (String.length ner.text) textFromNer

                            cnNer =
                                CNNer
                                    { begin = ner.start_char
                                    , end = ner.start_char + String.length textNer
                                    , ner = ner
                                    , children = computeToks ner.start_char textNer
                                    }
                        in
                        { children =
                            acc.children
                                ++ computeToks acc.parTextLeftPosAbs textBeforeNer
                                ++ [ cnNer ]
                        , parTextLeft = textAfterNer
                        , parTextLeftPosAbs = ner.start_char + String.length textNer
                        }

                foldRes =
                    List.foldl foldFunc
                        { children = []
                        , parTextLeft = parText
                        , parTextLeftPosAbs = parBeginAbs
                        }
                        (Maybe.withDefault [] ps.mSpacyNers)

                leftoverNodes =
                    computeToks foldRes.parTextLeftPosAbs foldRes.parTextLeft
            in
            foldRes.children ++ leftoverNodes

        computeToks : Int -> String -> List ContentNode
        computeToks parBeginAbs parText =
            let
                foldFunc pos acc =
                    if pos.idx < acc.parTextLeftPosAbs then
                        acc

                    else if pos.idx >= acc.parTextLeftPosAbs + String.length acc.parTextLeft then
                        acc

                    else
                        let
                            textBeforeTok =
                                String.left (pos.idx - acc.parTextLeftPosAbs) acc.parTextLeft

                            textFromTok =
                                String.dropLeft (pos.idx - acc.parTextLeftPosAbs) acc.parTextLeft

                            textNer =
                                String.left (String.length pos.text) textFromTok

                            textAfterTok =
                                String.dropLeft (String.length pos.text) textFromTok

                            cnBeforeTok =
                                CNText
                                    { begin = acc.parTextLeftPosAbs
                                    , end = String.length textBeforeTok
                                    , text = textBeforeTok
                                    }

                            cnTok =
                                CNTok
                                    { begin = pos.idx
                                    , end = pos.idx + String.length pos.text
                                    , tok = pos
                                    , children =
                                        [ CNText
                                            { begin = pos.idx
                                            , end = pos.idx + String.length pos.text
                                            , text = pos.text
                                            }
                                        ]
                                    }
                        in
                        { children = acc.children ++ [ cnBeforeTok, cnTok ]
                        , parTextLeft = textAfterTok
                        , parTextLeftPosAbs = pos.idx + String.length pos.text
                        }

                foldRes =
                    List.foldl foldFunc
                        { children = []
                        , parTextLeft = parText
                        , parTextLeftPosAbs = parBeginAbs
                        }
                        (Maybe.withDefault [] ps.mSpacyPoss)

                leftoverNode =
                    CNText
                        { begin = foldRes.parTextLeftPosAbs
                        , end = foldRes.parTextLeftPosAbs + String.length foldRes.parTextLeft
                        , text = foldRes.parTextLeft
                        }
            in
            foldRes.children ++ [ leftoverNode ]
    in
    paragraphs


renderContentNodes :
    { nersToHighlight : List String
    , selectedToken : Maybe Int -- tok.i
    , highlightPos : Bool
    , highlightAllNers : Bool
    , highlightSentiment : Bool
    , nodes : List ContentNode
    , onClickToken : Api.SpacyToken -> msg
    , onClickNer : Api.CmdSpacyNerResEnt -> msg
    , depChildren : Set Int
    , depParent : Maybe Int
    , mFasttextSentimentMap : Dict Int Api.FasttextSentiment
    }
    -> Html msg
renderContentNodes ps =
    let
        renderNode : ContentNode -> Html msg
        renderNode node =
            case node of
                CNText nd ->
                    span [ class "content-text" ] [ text nd.text ]

                CNParagraph nd ->
                    p [] <| List.map renderNode nd.children

                CNNer nd ->
                    let
                        selectedPersonHighlight =
                            nd.ner.label_ == "PERSON" && List.member nd.ner.text ps.nersToHighlight
                    in
                    span
                        [ class "content-ner"
                        , classList
                            [ ( "badge-highlighed-token badge-highlighed-token--ner"
                              , selectedPersonHighlight || ps.highlightAllNers
                              )
                            ]
                        , onClick <| ps.onClickNer nd.ner
                        ]
                    <|
                        List.map renderNode nd.children
                            ++ (if ps.highlightAllNers then
                                    [ text <| ":" ++ nd.ner.label_ ]

                                else
                                    []
                               )

                CNTok nd ->
                    let
                        isHighlighted =
                            Just nd.tok.i == ps.selectedToken

                        isParent =
                            Just nd.tok.i == ps.depParent

                        isChild =
                            not isParent && Set.member nd.tok.i ps.depChildren

                        sentimentClass =
                            if ps.highlightSentiment then
                                case Dict.get nd.tok.i ps.mFasttextSentimentMap of
                                    Just fasttextSentiment ->
                                        Just ("content-token--sentiment-" ++ sentimentSuffix fasttextSentiment.label)

                                    Nothing ->
                                        Nothing

                            else
                                Nothing

                        -- turns sentiment from [-1;+1] float range into one of "-ng-2", "-ng-1", "-neutral", "-pos-1", "-pos-2"
                        sentimentSuffix : Float -> String
                        sentimentSuffix x =
                            if x <= -0.5 then
                                "ng-2"

                            else if x < -0.1 then
                                "ng-1"

                            else if x < 0.1 then
                                "neutral"

                            else if x < 0.5 then
                                "pos-1"

                            else
                                "pos-2"

                        sentimentClassList =
                            case sentimentClass of
                                Nothing ->
                                    []

                                Just sentimentCls ->
                                    [ class <| sentimentCls ]
                    in
                    span [ class "content-token-wrap" ]
                        [ span
                            ([ class "content-token"
                             , classList
                                [ ( "content-token--" ++ nd.tok.pos_
                                  , ps.highlightPos
                                  )
                                , ( "badge-highlighed-token"
                                  , isHighlighted || isChild
                                  )
                                , ( "badge-highlighed-token--dep-child"
                                  , isChild
                                  )
                                , ( "badge-highlighed-token badge-highlighed-token--dep-parent"
                                  , isParent
                                  )
                                ]
                             , onClick <| ps.onClickToken nd.tok
                             ]
                                ++ sentimentClassList
                            )
                            (List.map renderNode nd.children)
                        ]
    in
    div [] <| List.map renderNode ps.nodes
