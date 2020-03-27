module Le.WebClient where

import qualified Data.ByteString.Lazy as BL
import qualified Le.ApiTypes as AT
import Le.Import
import Le.Routes
import Servant.Client
import Servant.Client.Generic

api :: Proxy (API (AsClientT (RIO App)))
api = Proxy

cliRoutes :: API (AsClientT ClientM)
cliRoutes =
  genericClientHoist
    (\x -> liftIO (runClientM x env >>= either throwIO return))
  where
    env = error "undefined environment"

cliPing :: ClientM Text
cliPing = __ping cliRoutes

cliDownloadAndFilter :: AT.DownloadAndFilterForm -> ClientM BL.ByteString
cliDownloadAndFilter = __downloadAndFilter cliRoutes
