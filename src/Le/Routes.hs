{-# LANGUAGE TypeOperators #-}

module Le.Routes where

import qualified Data.ByteString.Lazy as BL
import qualified Data.String.Class as S
import qualified Le.ApiTypes as AT
import Le.Handlers
import Le.Import
import Network.HTTP.Media ((//), (/:))
import Servant
import Servant.API.Generic
import Servant.Server.Generic

data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance S.ConvLazyByteString a => MimeRender HTML a where
  mimeRender _ val = S.toLazyByteString val

instance MimeUnrender HTML Text where
  mimeUnrenderWithType _ "text/html" bs = Right (S.toText bs)
  mimeUnrenderWithType _ mt _bs = Left $ "Mime must be text/html, but it's: " ++ show mt

data GZip

instance Accept GZip where
  contentType _ = "application" // "gzip"

instance MimeRender GZip BL.ByteString where
  mimeRender _ val = val

instance MimeUnrender GZip BL.ByteString where
  mimeUnrenderWithType _ "application/gzip" bs = Right bs
  mimeUnrenderWithType _ mt _bs = Left $ "Mime must be application/gzip, but it's: " ++ show mt

data API route
  = API
      { __index :: route :- Get '[HTML] Text,
        __ping :: route :- "api" :> "ping" :> Get '[PlainText] Text,
        __jsonApi :: route :- ToServantApi JsonAPI,
        __downloadAndFilter ::
          route
            :- "api" :> "download-and-filter.json"
              :> ReqBody '[JSON] AT.DownloadAndFilterForm
              :> Post '[GZip] BL.ByteString,
        __testDownloadAndFilter ::
          route
            :- "api" :> "test-download-and-filter.json"
              :> Post '[GZip] BL.ByteString
      }
  deriving (Generic)

data JsonAPI route
  = JsonAPI
      { _logErrorHandler ::
          route
            :- "api"
            :> "log-error.json"
            :> QueryParam "msg" Text
            :> Get '[JSON] (),
        _pingJson ::
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
    { __index = indexNoAuth,
      __ping = ping,
      __jsonApi = toServant jsonApi,
      __downloadAndFilter = downloadAndFilter,
      __testDownloadAndFilter = testDownloadAndFilter
    }
  where
    jsonApi :: JsonAPI (AsServerT (RIO App))
    jsonApi =
      JsonAPI
        { _logErrorHandler = logErrorHandler,
          _pingJson = pingJson,
          _errorOut = errorOut
        }
