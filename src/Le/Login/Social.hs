-- | For facebook app token generation see https://developers.facebook.com/docs/facebook-login/access-tokens/
module Le.Login.Social where

import Control.Lens ((.~))
import qualified Data.Aeson as J
import qualified Data.Binary.Builder
import qualified Data.String.Class as S
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID4
import qualified Database.Persist.Postgresql as P
import Le.App
import Le.AppUtils
import Le.Import
import qualified Le.Login.Handlers
import Le.Model
import qualified Network.Wreq as W
import Servant
import qualified Web.Cookie as Cookie

data FbOauthRsp = FbOauthRsp
  { fboAccessToken :: Text,
    fboTokenType :: Text,
    fboExpiresIn :: Int
  }
  deriving (Show, Eq, Generic)

instance J.ToJSON FbOauthRsp where
  toEncoding = J.genericToEncoding (jsonOpts 3)

instance J.FromJSON FbOauthRsp where
  parseJSON = J.genericParseJSON (jsonOpts 3)

data FbDebugTokenRsp = FbDebugTokenRsp
  { fbtData :: FbDebugTokenRspData
  }
  deriving (Show, Eq, Generic)

instance J.ToJSON FbDebugTokenRsp where
  toEncoding = J.genericToEncoding (jsonOpts 3)

instance J.FromJSON FbDebugTokenRsp where
  parseJSON = J.genericParseJSON (jsonOpts 3)

data FbDebugTokenRspData = FbDebugTokenRspData
  { fbdUserId :: Text
  }
  deriving (Show, Eq, Generic)

instance J.ToJSON FbDebugTokenRspData where
  toEncoding = J.genericToEncoding (jsonOpts 3)

instance J.FromJSON FbDebugTokenRspData where
  parseJSON = J.genericParseJSON (jsonOpts 3)

data FbUser = FbUser
  { fbuName :: Text,
    fbuEmail :: Text,
    fbuPicture :: FbPicture
  }
  deriving (Show, Eq, Generic)

instance J.ToJSON FbUser where
  toEncoding = J.genericToEncoding (jsonOpts 3)

instance J.FromJSON FbUser where
  parseJSON = J.genericParseJSON (jsonOpts 3)

data FbPicture = FbPicture
  { fbpData :: FbPictureData
  }
  deriving (Show, Eq, Generic)

instance J.ToJSON FbPicture where
  toEncoding = J.genericToEncoding (jsonOpts 3)

instance J.FromJSON FbPicture where
  parseJSON = J.genericParseJSON (jsonOpts 3)

data FbPictureData = FbPictureData
  { fpdUrl :: Text,
    fpdHeight :: Int,
    fpdWidth :: Int
  }
  deriving (Show, Eq, Generic)

instance J.ToJSON FbPictureData where
  toEncoding = J.genericToEncoding (jsonOpts 3)

instance J.FromJSON FbPictureData where
  parseJSON = J.genericParseJSON (jsonOpts 3)

fbLoginCallbackEndpoint ::
  HasCallStack =>
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  AppM (Headers '[Header "Set-Cookie" Cookie.SetCookie] Text)
fbLoginCallbackEndpoint mErrorCode mErrorMessage mCode _mState =
  sg $ do
    cfg <- asks appConfig
    case mErrorMessage of
      Just errorMsg ->
        error $
          "Got error from Facebook: " <> S.toString errorMsg <> ". Code: "
            <> show mErrorCode
      Nothing -> do
        mgr <- asks appHttpManager
        let opts = W.defaults & W.manager .~ Right mgr
        code <- mustBeJust "Facebook callback code is empty" mCode
        let opts1 =
              opts & W.param "client_id" .~ [cfgFacebookAppId cfg]
                & W.param "redirect_uri"
                  .~ ["https://meetup.events/api/fb-login-callback"]
                & W.param "client_secret" .~ [cfgFacebookAppSecret cfg]
                & W.param "code" .~ [code]
        res1 <-
          liftIO $
            W.getWith opts1 "https://graph.facebook.com/v3.2/oauth/access_token"
        oauth <-
          mustBeRight
            (\e -> "Error parsing Facebook Oauth response: " <> tshow e)
            (J.eitherDecode (res1 ^. W.responseBody))
        let opts2 =
              opts & W.param "input_token" .~ [fboAccessToken oauth]
                & W.param "access_token" .~ [cfgFacebookAppToken cfg]
        res2 <-
          liftIO $ W.getWith opts2 "https://graph.facebook.com/debug_token"
        debugTokenRsp <-
          mustBeRight
            (\e -> "Error parsing Facebook debug_token response: " <> tshow e)
            (J.eitherDecode (res2 ^. W.responseBody))
        let opts3 =
              opts & W.param "fields" .~ ["id,name,email,picture"]
                & W.param "access_token" .~ [fboAccessToken oauth]
        res3 <-
          liftIO $
            W.getWith
              opts3
              ( S.fromText
                  ( "https://graph.facebook.com/v3.2/"
                      <> fbdUserId (fbtData debugTokenRsp)
                      <> "/"
                  )
              )
        logInfo
          $ display
          $ "Success! res3: " <> S.toText (res3 ^. W.responseBody)
        fbUserRsp <-
          mustBeRight
            (\e -> "Error parsing Facebook user data: " <> tshow e)
            (J.eitherDecode (res3 ^. W.responseBody))
        logInfo
          $ display
          $ "Success! Got user info: " <> S.toText (J.encode fbUserRsp)
        t <- liftIO getCurrentTime
        user <- runDb $ Le.Login.Handlers.blGetOrCreateByEmail t (fbuEmail fbUserRsp)
        tval <- liftIO $ UUID.toText <$> UUID4.nextRandom
        _ <-
          runDb $
            P.insert
              LoginToken
                { loginTokenTokenVal = pack (tval),
                  loginTokenUserId = entityKey user,
                  loginTokenCreatedAt = t
                }
        let inAMonth = addUTCTime (60 * 60 * 24 * 30) t
        let cookie =
              Cookie.defaultSetCookie
                { Cookie.setCookieName = "u",
                  Cookie.setCookieValue = S.fromText tval,
                  Cookie.setCookiePath = Just "/",
                  Cookie.setCookieExpires = Just inAMonth
                }
        throwM $
          err302
            { errHeaders =
                [ ( "Set-Cookie",
                    S.toStrictByteString
                      ( Data.Binary.Builder.toLazyByteString
                          (Cookie.renderSetCookie cookie)
                      )
                  ),
                  ("Location", "https://meetup.events/account")
                ]
            }
  where
    mustBeJust :: Text -> Maybe a -> AppM a
    mustBeJust err mv =
      case mv of
        Nothing -> errExit err
        Just v -> pure v
    mustBeRight :: (String -> Text) -> Either String a -> AppM a
    mustBeRight onErr ev' =
      case ev' of
        Left e -> errExit (onErr e)
        Right v -> pure v
    errExit :: Text -> AppM a
    errExit e = do
      logError (display e)
      error "Error parsing Facebook response"

data GoogleOauthRsp = GoogleOauthRsp
  { gorAccessToken :: Text,
    gorExpiresIn :: Int,
    gorTokenType :: Text,
    gorRefreshToken :: Maybe Text,
    gorScope :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance J.ToJSON GoogleOauthRsp where
  toEncoding = J.genericToEncoding (jsonOpts 3)

instance J.FromJSON GoogleOauthRsp where
  parseJSON = J.genericParseJSON (jsonOpts 3)

data GoogleUserinfo = GoogleUserinfo
  { gouId :: Text,
    gouEmail :: Text,
    gouVerifiedEmail :: Bool,
    gouName :: Text,
    gouGivenName :: Text,
    gouFamilyName :: Text,
    gouPicture :: Text
  }
  deriving (Show, Eq, Generic)

instance J.ToJSON GoogleUserinfo where
  toEncoding = J.genericToEncoding (jsonOpts 3)

instance J.FromJSON GoogleUserinfo where
  parseJSON = J.genericParseJSON (jsonOpts 3)

googleLoginCallback ::
  HasCallStack =>
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  AppM (Headers '[Header "Set-Cookie" Cookie.SetCookie] Text)
googleLoginCallback _mState mCode _mScope mError =
  sg $ do
    cfg <- asks appConfig
    case mError of
      Just errorMsg -> do
        error $ "Got error from Google: " <> S.toString errorMsg
      Nothing -> do
        code <- mustBeJust "Got empty code callback from google" mCode
        mgr <- asks appHttpManager
        let opts = W.defaults & W.manager .~ Right mgr
        res <-
          liftIO $
            W.postWith
              opts
              "https://www.googleapis.com/oauth2/v4/token"
              [ "code" W.:= code,
                "client_id" W.:= cfgGoogleOauthClientId cfg,
                "client_secret" W.:= cfgGoogleOauthClientSecret cfg,
                "redirect_uri"
                  W.:= ("https://meetup.events/api/google-login-callback" :: Text),
                "grant_type" W.:= ("authorization_code" :: Text)
              ]
        logInfo
          $ display
          $ "Got response body: " <> (S.toText (res ^. W.responseBody))
        oauth <-
          mustBeRight
            (\e -> "Error parsing Google response: " <> tshow e)
            (J.eitherDecode (res ^. W.responseBody))
        let opts2 =
              opts
                & W.header "Authorization"
                .~ ["Bearer " <> S.fromText (gorAccessToken oauth)]
        res2 <-
          liftIO $
            W.getWith
              opts2
              "https://www.googleapis.com/oauth2/v1/userinfo?alt=json"
        logInfo
          $ display
          $ "Got response body: " <> (S.toText (res2 ^. W.responseBody))
        googleUserInfo <-
          mustBeRight
            (\e -> "Error parsing user info: " <> S.toText e)
            (J.eitherDecode (res2 ^. W.responseBody))
        logInfo
          $ display
          $ "Success! Got user info: " <> S.toText (J.encode googleUserInfo)
        t <- liftIO getCurrentTime
        user <- runDb $ Le.Login.Handlers.blGetOrCreateByEmail t (gouEmail googleUserInfo)
        tval <- liftIO $ UUID.toText <$> UUID4.nextRandom
        _ <-
          runDb $
            P.insert
              LoginToken
                { loginTokenTokenVal = pack (tval),
                  loginTokenUserId = entityKey user,
                  loginTokenCreatedAt = t
                }
        let inAMonth = addUTCTime (60 * 60 * 24 * 30) t
        let cookie =
              Cookie.defaultSetCookie
                { Cookie.setCookieName = "u",
                  Cookie.setCookieValue = S.fromText tval,
                  Cookie.setCookiePath = Just "/",
                  Cookie.setCookieExpires = Just inAMonth
                }
        throwM $
          err302
            { errHeaders =
                [ ( "Set-Cookie",
                    S.toStrictByteString
                      ( Data.Binary.Builder.toLazyByteString
                          (Cookie.renderSetCookie cookie)
                      )
                  ),
                  ("Location", "https://meetup.events/account")
                ]
            }
  where
    mustBeJust :: Text -> Maybe a -> AppM a
    mustBeJust err mv =
      case mv of
        Nothing -> errExit err
        Just v -> pure v
    mustBeRight :: (String -> Text) -> Either String a -> AppM a
    mustBeRight onErr ev' =
      case ev' of
        Left e -> errExit (onErr e)
        Right v -> pure v
    errExit :: Text -> AppM a
    errExit e = do
      logError (display e)
      error "Error parsing Google response"
