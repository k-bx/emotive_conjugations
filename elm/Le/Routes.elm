module Le.Routes exposing (..)

import Le.Api as Api
import Le.Config exposing (..)
import Le.Utils exposing (..)
import Time
import Url.Builder


dashboard : String
dashboard =
    "/dashboard"
