port module Le.Ports exposing (..)

import Json.Encode as E


-- port initTooltips : () -> Cmd msg


-- port tooltipShow : { id : String } -> Cmd msg


-- port tooltipHide : { id : String } -> Cmd msg


port needLoginRedirect : () -> Cmd msg


port redirectBackAfterLogin : () -> Cmd msg


-- port scrollIntoView : { query : String } -> Cmd msg
