{-# LANGUAGE TypeOperators #-}

module Le.Routes where

import qualified Data.ByteString.Lazy as BL
import qualified Le.ApiTypes as AT
import Le.Handlers
import Le.Import
import Network.HTTP.Media ((//))
import Servant
import Servant.API.Generic
import Servant.Server.Generic

data GZip

instance Accept GZip where
  contentType _ = "application" // "gzip"

instance MimeRender GZip BL.ByteString where
  mimeRender _ val = val

instance MimeUnrender GZip BL.ByteString where
  mimeUnrender _ bs =
    case BL.take (fromIntegral (length ("application/gzip" :: String))) bs of
      "application/gzip" -> Right ""
      _ -> Left "Mime must be application/gzip"

data API route
  = API
      { __ping :: route :- "api" :> "ping" :> Get '[PlainText] Text,
        __jsonApi :: route :- ToServantApi JsonAPI,
        __downloadAndFilter ::
          route
            :- "api" :> "download-and-filter.json"
              :> ReqBody '[JSON] AT.DownloadAndFilterForm
              :> Post '[GZip] BL.ByteString
      }
  deriving (Generic)

data JsonAPI route
  = JsonAPI
      { _pingJson ::
          route
            :- "api" :> "ping.json" :> Get '[JSON] [Text],
        _errorOut ::
          route
            :- "api" :> "error-out.json" :> Get '[JSON] [Text]
      }
  deriving (Generic)

server :: API (AsServerT (RIO App))
server =
  API
    { __ping = ping,
      __jsonApi = toServant jsonApi,
      __downloadAndFilter = downloadAndFilter
    }
  where
    jsonApi :: JsonAPI (AsServerT (RIO App))
    jsonApi =
      JsonAPI
        { _pingJson = pingJson,
          _errorOut = errorOut
        }
