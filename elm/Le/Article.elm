module Le.Article exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Le.Api as Api
import Maybe.Extra


{-| Converts `"\n\n"` to paragraphs, highlights NERs and stuff
-}
renderContent :
    String
    -> String
    -> Maybe (List Api.CmdSpacyNerResEnt)
    -> Maybe (List Api.CmdSpacyPosResEnt)
    -> List (Html msg)
renderContent nerToHighlight inputText mSpacyNers mSpacyPoss =
    let
        spacyNers =
            Maybe.withDefault [] mSpacyNers

        spacyPoss =
            Maybe.withDefault [] mSpacyPoss

        -- beginnings of various things to be wrapped in spans later
        dotsList : List Int
        dotsList =
            List.sort
                (List.concatMap
                    (\x ->
                        [ x.idx
                        , x.idx + String.length x.text
                        ]
                    )
                    spacyPoss
                )
                ++ List.concatMap
                    (\x ->
                        [ x.start_char
                        , x.start_char + String.length x.text
                        ]
                    )
                    spacyNers

        -- map from dot to potential token info
        tokMap : Dict Int Api.CmdSpacyPosResEnt
        tokMap =
            Dict.fromList
                (List.map
                    (\x ->
                        ( x.idx, x )
                    )
                    spacyPoss
                )

        -- map from dot to potential NER info
        nerMap : Dict Int Api.CmdSpacyNerResEnt
        nerMap =
            Dict.fromList
                (List.map
                    (\x ->
                        ( x.start_char, x )
                    )
                    spacyNers
                )

        go textLeft dotsLeft consumed mTokTill mNerTill =
            case dotsLeft of
                [] ->
                    []

                dot :: dots ->
                    let
                        dot2 =
                            case dots of
                                [] ->
                                    dot + String.length textLeft

                                z :: _ ->
                                    z

                        cutLen =
                            dot2 - dot

                        currCut =
                            String.left cutLen textLeft

                        afterCut =
                            String.dropLeft cutLen textLeft

                        mContinuingTok =
                            case mTokTill of
                                Nothing ->
                                    Nothing

                                Just tok ->
                                    case tok.idx + String.length tok.text > dot of
                                        True ->
                                            Just tok

                                        False ->
                                            Nothing

                        mOpeningTok =
                            Dict.get dot tokMap
                                |> Maybe.Extra.orElse mContinuingTok

                        mContinuingNer =
                            case mNerTill of
                                Nothing ->
                                    Nothing

                                Just ner ->
                                    case ner.start_char + String.length ner.text > dot of
                                        True ->
                                            Just ner

                                        False ->
                                            Nothing

                        mOpeningNer =
                            Dict.get dot nerMap
                                |> Maybe.Extra.orElse mContinuingNer

                        el0 =
                            text currCut

                        el1 =
                            mOpeningTok |> Maybe.Extra.unwrap el0 (\openingTok -> wrapInTok openingTok el0)

                        el2 =
                            mOpeningNer |> Maybe.Extra.unwrap el1 (\openingNer -> wrapInNer openingNer el1)
                    in
                    [ el2 ] ++ go afterCut dots (consumed + cutLen) mOpeningTok mOpeningNer

        wrapInTok tok el =
            span [ class "content-token" ]
                [ el
                -- , span [ class "content-token__pos" ]
                --     [ text <| tok.pos_
                --     ]
                ]

        wrapInNer ner el =
            span
                [ class "content-ner"
                , classList
                    [ ( "badge badge-info article__spacy-ner"
                      , ner.label_ == "PERSON" && ner.text == nerToHighlight
                      )
                    ]
                ]
                [ el ]
    in
    go inputText dotsList 0 Nothing Nothing
