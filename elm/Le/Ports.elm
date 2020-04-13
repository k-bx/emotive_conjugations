port module Le.Ports exposing (..)

import Json.Encode as E


port needLoginRedirect : () -> Cmd msg


port redirectBackAfterLogin : () -> Cmd msg


port initTooltips : () -> Cmd msg


-- port tooltipToggle : { id : String } -> Cmd msg


-- port tooltipShow : { id : String } -> Cmd msg


-- port tooltipHide : { id : String } -> Cmd msg
