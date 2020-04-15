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

        -- , ner : { text : String, start_char : Int, label_ : String }
        , ner : Api.CmdSpacyNerResEnt
        , children : List ContentNode
        }
    | CNTok
        { begin : Int
        , end : Int
        , tok : Api.CmdSpacyPosResEnt

        -- , tok : { idx : Int, pos_ : String }
        , children : List ContentNode
        }


computeContentNodes :
    { inputText : String
    , mSpacyNers : Maybe (List Api.CmdSpacyNerResEnt)
    , mSpacyPoss : Maybe (List Api.CmdSpacyPosResEnt)
    }
    -> List ContentNode
computeContentNodes ps =
    let
        parEndsDots =
            String.indexes "\n\n" (ps.inputText ++ "\n\n")

        paragraphs : List ContentNode
        paragraphs =
            List.Extra.zip ([ -2 ] ++ parEndsDots) parEndsDots
                |> List.map
                    (\( prevParEnd, parEnd ) ->
                        let
                            begin =
                                prevParEnd + 2

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

                                    -- , ner = { text = textNer, start_char = ner.start_char, label_ = ner.label_ }
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

                                    -- , tok = { idx = pos.idx, pos_ = pos.pos_ }
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
    { nerToHighlight : String
    , selectedToken : Maybe Int -- tok.i
    , highlightPos : Bool
    , nodes : List ContentNode
    , onClickToken : Api.CmdSpacyPosResEnt -> msg
    , onClickNer : Api.CmdSpacyNerResEnt -> msg
    , depChildren : Set Int
    , depParent : Maybe Int
    }
    -> Html msg
renderContentNodes ps =
    let
        renderNode node =
            case node of
                CNText nd ->
                    span [ class "content-text" ] [ text nd.text ]

                CNParagraph nd ->
                    p [] <| List.map renderNode nd.children

                CNNer nd ->
                    span
                        [ class "content-ner"
                        , classList
                            [ ( "badge-highlighed-token badge-highlighed-token--ner"
                              , nd.ner.label_ == "PERSON" && nd.ner.text == ps.nerToHighlight
                              )
                            ]
                        , onClick <| ps.onClickNer nd.ner
                        ]
                        (List.map renderNode nd.children)

                CNTok nd ->
                    span [ class "content-token-wrap" ]
                        [ span
                            [ class "content-token"
                            , classList
                                [ ( "content-token--" ++ nd.tok.pos_
                                  , ps.highlightPos
                                  )
                                , ( "badge-highlighed-token"
                                  , Just nd.tok.i == ps.selectedToken
                                  )
                                , ( "badge-highlighed-token badge-highlighed-token--dep-parent"
                                  , Just nd.tok.i == ps.depParent
                                  )
                                , ( "badge-highlighed-token badge-highlighed-token--dep-child"
                                  , Set.member nd.tok.i ps.depChildren
                                  )
                                ]
                            , onClick <| ps.onClickToken nd.tok
                            ]
                            (List.map renderNode nd.children)
                        ]
    in
    div [] <| List.map renderNode ps.nodes
