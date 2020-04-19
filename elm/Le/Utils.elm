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


{-| Example: "JUN 22 2019" on timeline
-}
renderDateTimeline : Time.Posix -> String
renderDateTimeline x =
    let
        mon =
            Time.toMonth Time.utc x

        day =
            Time.toDay Time.utc x

        year =
            Time.toYear Time.utc x

        yearMinus2k =
            year - 2000
    in
    renderMonthShort mon
        ++ "/"
        ++ intToStringTwoSigns day
        ++ "/"
        ++ String.fromInt yearMinus2k


renderDateInfobox : Time.Posix -> String
renderDateInfobox x =
    let
        mon =
            Time.toMonth Time.utc x

        day =
            Time.toDay Time.utc x

        year =
            Time.toYear Time.utc x

        hour24 =
            Time.toHour Time.utc x

        hour12 =
            if hour24 >= 12 then
                hour24 - 12

            else
                hour24

        ampm =
            if hour24 >= 12 then
                "PM"

            else
                "AM"
    in
    renderMonthShort mon
        ++ " "
        ++ intToStringTwoSigns day
        ++ ", "
        ++ String.fromInt year
        ++ " "
        ++ String.fromInt hour12
        ++ ":"
        ++ String.fromInt (Time.toMinute Time.utc x)
        ++ " "
        ++ ampm


renderMonthShort : Time.Month -> String
renderMonthShort m =
    case m of
        Jan ->
            "Jan"

        Feb ->
            "Feb"

        Mar ->
            "Mar"

        Apr ->
            "Apr"

        May ->
            "May"

        Jun ->
            "Jun"

        Jul ->
            "Jul"

        Aug ->
            "Aug"

        Sep ->
            "Sep"

        Oct ->
            "Oct"

        Nov ->
            "Nov"

        Dec ->
            "Dec"


intToStringTwoSigns : Int -> String
intToStringTwoSigns x =
    if x < 10 then
        "0" ++ String.fromInt x

    else
        String.fromInt x


boolYesNo : Bool -> String
boolYesNo x =
    case x of
        True ->
            "Yes"

        False ->
            "No"
