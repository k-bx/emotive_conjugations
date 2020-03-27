module Le.WebClient where

import Le.Import
import Le.Routes
import Servant.API.Generic
import Servant.Client
import Servant.Client.Generic

api :: Proxy (API (AsClientT (RIO App)))
api = Proxy

api2 :: Proxy (ToServantApi API)
api2 = Proxy

cliRoutes :: API (AsClientT IO)
cliRoutes =
  genericClientHoist
    (\x -> runClientM x env >>= either throwIO return)
  where
    env = error "undefined environment"

cliPing :: IO Text
cliPing = __ping cliRoutes
