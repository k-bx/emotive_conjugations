module Le.Article exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Le.Api as Api


{-| Converts `"\n\n"` to paragraphs, highlights NERs and stuff
-}
renderContent :
    String
    -> Maybe (List Api.CmdSpacyNerResEnt)
    -> List (Html msg)
renderContent inputText spacyNers =
    let
        highlightNers textLeft consumed ners =
            case ners of
                [] ->
                    renderRegularText textLeft

                n :: ns ->
                    case n.start_char == consumed of
                        True ->
                            let
                                l =
                                    n.end_char - n.start_char
                            in
                            renderNer n
                                ++ highlightNers (String.dropLeft l textLeft) (consumed + l) ns

                        False ->
                            let
                                l =
                                    n.start_char - consumed
                            in
                            renderRegularText (String.left l textLeft)
                                ++ highlightNers (String.dropLeft l textLeft) (consumed + l) ners

        renderRegularText s =
            s
                |> String.lines
                |> List.map text
                |> List.intersperse (br [] [])

        renderNer n =
            [ span [ class "article__spacy-ner" ] [ text n.text ] ]
    in
    highlightNers inputText 0 (Maybe.withDefault [] spacyNers)
