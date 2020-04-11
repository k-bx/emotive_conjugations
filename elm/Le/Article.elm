module Le.Article exposing (..)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Le.Api as Api
import List.Extra
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
    -- [text inputText]
    let
        spacyNers =
            Maybe.withDefault [] mSpacyNers

        spacyPoss =
            Maybe.withDefault [] mSpacyPoss

        -- beginnings of various things to be wrapped in spans later
        dotsList : List Int
        dotsList =
            List.Extra.unique
                (List.sort
                    (List.concatMap
                        (\x ->
                            [ x.idx
                            , x.idx + String.length x.text
                            ]
                        )
                        spacyPoss
                        ++ List.concatMap
                            (\x ->
                                [ x.start_char
                                , x.start_char + String.length x.text
                                ]
                            )
                            spacyNers
                    )
                )

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

        wrapInTok tok el =
            span [ class "content-token-wrap" ]
                [ span
                    [ class "content-token"
                    , classList [ ( "content-token--" ++ tok.pos_, True ) ]
                    ]
                    [ el
                    ]
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

        foldfunc ( dot, dot2 ) acc =
            let
                cutLen =
                    dot2 - dot

                currCut =
                    String.left cutLen acc.textLeft

                afterCut =
                    String.dropLeft cutLen acc.textLeft

                mContinuingTok =
                    case acc.mTokTill of
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
                    case acc.mNerTill of
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
            { acc
                | resAcc = el2 :: acc.resAcc
                , textLeft = afterCut
                , mTokTill = mOpeningTok
                , mNerTill = mOpeningNer
            }

        res =
            List.foldl foldfunc
                { textLeft = inputText
                , mTokTill = Nothing
                , mNerTill = Nothing
                , resAcc = []
                }
                (List.Extra.zip dotsList (List.drop 1 dotsList ++ [ String.length inputText ]))
    in
    List.reverse res.resAcc
