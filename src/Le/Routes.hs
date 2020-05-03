{-# LANGUAGE TypeOperators #-}

module Le.Routes where

import qualified Data.ByteString.Lazy as BL
import qualified Data.String.Class as S
import qualified Le.ApiTypes as AT
import Le.Article.Handlers
import Le.Handlers
import Le.Import
import Le.Login.Handlers
import Le.Login.Social
import Le.Model
import Le.Queue.Handlers
import Network.HTTP.Media ((//), (/:))
import Servant
import Servant.API.Generic
import Servant.Server.Experimental.Auth (AuthServerData)
import Servant.Server.Generic
import Web.Cookie (SetCookie)

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

-- | We need to specify the data returned after authentication
type instance
  AuthServerData (AuthProtect "cookie-auth") =
    Entity User

data API route = API
  { __index :: route :- Get '[HTML] Text,
    __dashboard :: route :- "dashboard" :> Get '[HTML] Text,
    __queue :: route :- AuthProtect "cookie-auth" :> "queue" :> Get '[HTML] Text,
    __login :: route :- "login" :> Get '[HTML] Text,
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
          :> Post '[GZip] BL.ByteString,
    __fbLoginCallback ::
      route
        :- "api"
        :> "fb-login-callback"
        :> QueryParam "error_code" Text
        :> QueryParam "error_message" Text
        :> QueryParam "code" Text
        :> QueryParam "state" Text
        :> Get '[HTML] (Headers '[Header "Set-Cookie" SetCookie] Text),
    __googleLoginCallback ::
      route
        :- "api"
        :> "google-login-callback"
        :> QueryParam "state" Text
        :> QueryParam "code" Text
        :> QueryParam "scope" Text
        :> QueryParam "error" Text
        :> Get '[HTML] (Headers '[Header "Set-Cookie" SetCookie] Text)
  }
  deriving (Generic)

data JsonAPI route = JsonAPI
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
        :- "api" :> "error-out.json" :> Get '[JSON] [Text],
    _articlesShortHandler ::
      route
        :- "api"
          :> "articles-short.json"
          :> QueryParam "person" Text
          :> Get '[JSON] [AT.ArticleShort],
    _articleDetails ::
      route
        :- "api" :> "article"
          :> Capture "article-id" ArticleId
          :> "article.json"
          :> Get '[JSON] AT.Article,
    _articlePleaseDetails ::
      route
        :- "api" :> "article"
          :> Capture "article-please-id" ArticlePleaseId
          :> "article-please.json"
          :> Get '[JSON] AT.ArticlePlease,
    _articlePleaseDetailsBig ::
      route
        :- "api" :> "article"
          :> Capture "article-please-big-id" ArticlePleaseBigId
          :> "article-please-big.json"
          :> Get '[JSON] AT.ArticlePleaseBig,
    _listNamedEntities ::
      route
        :- "api" :> "person-named-entities-list.json"
          :> QueryParam "q" Text
          :> QueryParam "page" Int
          :> Get '[JSON] (AT.Paginated Text),
    _namedEntityGroup ::
      route
        :- "api" :> "ner-group.json"
          :> QueryParam "ner" Text
          :> Get '[JSON] AT.NamedEntityGroup,
    _queueAdd ::
      route
        :- AuthProtect "cookie-auth"
        :> "api"
        :> "queue"
        :> "add.json"
        :> ReqBody '[JSON] AT.QueueAddForm
        :> Post '[JSON] (),
    _queueList ::
      route
        :- AuthProtect "cookie-auth"
        :> "api"
        :> "queue.json"
        :> Get '[JSON] [AT.QueueItem],
    _logInSendPassword ::
      route
        :- "api"
        :> "log-in-send-password"
        :> ReqBody '[JSON] AT.LogInSendPasswordForm
        :> Post '[JSON] (),
    _logInSendCode ::
      route
        :- "api"
        :> "log-in"
        :> ReqBody '[JSON] AT.LogInSendCodeForm
        :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie] AT.AccountInfo),
    _logOut ::
      route
        :- "api"
        :> "log-out"
        :> Get '[JSON] (Headers '[Header "Set-Cookie" SetCookie] ()),
    _accountInfo ::
      route :- AuthProtect "cookie-auth"
        :> "api"
        :> "account-info.json"
        :> Get '[JSON] AT.AccountInfo
  }
  deriving (Generic)

server :: API (AsServerT (RIO Env))
server =
  API
    { __index = throwM $ err302 {errHeaders = [("Location", "/dashboard")]},
      __dashboard = indexNoAuth,
      __queue = index,
      __login = indexNoAuth,
      __ping = ping,
      __jsonApi = toServant jsonApi,
      __downloadAndFilter = downloadAndFilter,
      __testDownloadAndFilter = testDownloadAndFilter,
      __fbLoginCallback = fbLoginCallbackEndpoint,
      __googleLoginCallback = googleLoginCallback
    }
  where
    jsonApi :: JsonAPI (AsServerT (RIO Env))
    jsonApi =
      JsonAPI
        { _logErrorHandler = logErrorHandler,
          _pingJson = pingJson,
          _errorOut = errorOut,
          _articlesShortHandler = articlesShortHandler,
          _articleDetails = articleDetails,
          _articlePleaseDetailsBig = articlePleaseDetailsBig,
          _articlePleaseDetails = articlePleaseDetails,
          _listNamedEntities = listNamedEntities,
          _namedEntityGroup = namedEntityGroup,
          _queueAdd = queueAdd,
          _queueList = queueList,
          _logInSendPassword = logInSendPasswordEndpoint,
          _logInSendCode = logInSendCodeEndpoint,
          _logOut = logOut,
          _accountInfo = accountInfo
        }
