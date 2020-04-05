port module Le.Ports exposing (..)

import Json.Encode as E


port needLoginRedirect : () -> Cmd msg


port redirectBackAfterLogin : () -> Cmd msg
