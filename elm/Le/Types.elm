module Le.Types exposing
    ( DayString
    , ViewParams
    )

import Le.Api as Api
import Time


type alias ViewParams =
    { isMobile : Bool
    , now : Maybe Time.Posix
    }


{-| ala "20190702"
-}
type alias DayString =
    String
