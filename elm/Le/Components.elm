module Le.Components exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


loadingSpinner =
    div
        [ class "spinner-border text-primary"
        , attribute "role" "status"
        ]
        [ span [ class "sr-only" ] [ text "Loading..." ]
        ]
