module Le.WebClient where

import qualified Data.ByteString.Lazy as BL
import qualified Le.ApiTypes as AT
import Le.Import
import Le.Routes
import Servant.Client
import Servant.Client.Generic

api :: Proxy (API (AsClientT (RIO App)))
api = Proxy

cliRoutes :: ClientEnv -> API (AsClientT Le)
cliRoutes env =
  genericClientHoist
    (\x -> liftIO (runClientM x env >>= either throwIO return))

cliPing :: ClientEnv -> Le Text
cliPing env = __ping (cliRoutes env)

cliDownloadAndFilter :: ClientEnv -> AT.DownloadAndFilterForm -> Le BL.ByteString
cliDownloadAndFilter env = __downloadAndFilter (cliRoutes env)