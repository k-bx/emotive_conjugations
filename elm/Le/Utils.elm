module Le.Utils exposing (..)

import Browser.Dom exposing (Viewport)
import Browser.Navigation
import Calendar
import Clock
import DateTime
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Json
import Le.Api as Api
import Le.Config exposing (..)
import Le.Types exposing (..)
import List.Extra
import Time exposing (Month(..))


isMobile : Viewport -> Bool
isMobile v =
    -- case v.viewport.width >= 1250 of
    case v.viewport.width >= 960 of
        True ->
            False

        False ->
            True
