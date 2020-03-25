module Le.Handlers where

import Le.AppUtils
import Le.Import

ping :: HasCallStack => RIO App Text
ping = sg $ return "pong"

pingJson :: HasCallStack => RIO App [Text]
pingJson = sg $ do pure $ ["pong"]

errorOut :: HasCallStack => RIO App [Text]
errorOut = sg $ do pure $ error "Something bad happened"
