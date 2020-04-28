module Le.Login.Handlers where

import qualified Data.Binary.Builder
import qualified Data.Char
import qualified Data.String.Class as S
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID4
import qualified Database.Persist.Postgresql as P
import qualified Le.ApiTypes as AT
import Le.App
import Le.AppUtils
import qualified Le.Config
import qualified Le.Emails
import Le.Import
import Le.Model
import Servant
import qualified System.Random
import qualified Web.Cookie as Cookie

logInSendPasswordEndpoint :: AT.LogInSendPasswordForm -> AppM ()
logInSendPasswordEndpoint AT.LogInSendPasswordForm {..} = do
  code <-
    liftIO $ fmap (tshow @Int) $
      System.Random.randomRIO
        (100000, 999999)
  logInfo $
    "Generated code for email: " <> display lisEmail <> "; code: "
      <> display code
  t <- liftIO $ getCurrentTime
  _ <-
    runDb $
      P.insert
        LoginCode
          { loginCodeCode = code,
            loginCodeCreatedAt = t,
            loginCodeEmail = lisEmail
          }
  Le.Emails.sendEmail
    Le.Emails.signInCodeTitle
    (Le.Emails.signInCodeTxt code)
    (Le.Emails.signInCodeHtml code)
    lisEmail
  return ()

logInSendCodeEndpoint ::
  AT.LogInSendCodeForm ->
  AppM (Headers '[Header "Set-Cookie" Cookie.SetCookie] AT.AccountInfo)
logInSendCodeEndpoint AT.LogInSendCodeForm {..} = do
  mLoginCode <-
    runDb $
      P.selectFirst
        [LoginCodeCode P.==. licCode, LoginCodeEmail P.==. licEmail]
        []
  case mLoginCode of
    Nothing -> noHeader <$> formErrors [("code", badCode)]
    Just loginCode -> do
      t <- liftIO getCurrentTime
      if addUTCTime
        Le.Config.loginCodeExpirationPeriod
        (loginCodeCreatedAt (P.entityVal loginCode))
        > t
        then do
          tval <- liftIO $ UUID.toText <$> UUID4.nextRandom
          user <-
            runDb $ do
              user <- blGetOrCreateByEmail t licEmail
              _ <-
                P.insert
                  LoginToken
                    { loginTokenTokenVal = pack (tval),
                      loginTokenUserId = P.entityKey user,
                      loginTokenCreatedAt = t
                    }
              pure user
          let cookie =
                Cookie.defaultSetCookie
                  { Cookie.setCookieName = "u",
                    Cookie.setCookieValue = S.fromText tval,
                    Cookie.setCookiePath = Just "/"
                  }
          accInfo <- renderAccountInfo user
          return $ addHeader cookie accInfo
        else noHeader <$> formErrors [("code", badCode)]
  where
    badCode = "Wrong sign-in code"

logOut ::
  HasCallStack => AppM (Headers '[Header "Set-Cookie" Cookie.SetCookie] ())
logOut = sg $ do
  t <- liftIO getCurrentTime
  let back = addUTCTime (-1000000) t
  let cookie =
        ( Cookie.defaultSetCookie
            { Cookie.setCookieName = "u",
              Cookie.setCookieValue = "",
              Cookie.setCookieExpires = Just back,
              Cookie.setCookiePath = Just "/"
            }
        )
  throwM $
    err302
      { errHeaders =
          [ ( "Set-Cookie",
              S.toStrictByteString
                ( Data.Binary.Builder.toLazyByteString
                    (Cookie.renderSetCookie cookie)
                )
            ),
            ("Location", "/")
          ]
      }

renderAccountInfo :: Entity User -> AppM AT.AccountInfo
renderAccountInfo user = do
  pure $
    AT.AccountInfo
      { accId = entityKey user,
        accEmail = userEmail (ev user)
      }

blGetOrCreateByEmail ::
  MonadIO m => UTCTime -> Text -> ReaderT P.SqlBackend m (P.Entity User)
blGetOrCreateByEmail t email = do
  mUser <- P.selectFirst [UserEmail P.==. email] []
  case mUser of
    Nothing -> do
      userKey <-
        P.insert
          User
            { userEmail = email,
              userCreatedAt = t,
              userUpdatedAt = t
            }
      P.getJustEntity userKey
    Just user -> pure user

accountInfo :: P.Entity User -> AppM AT.AccountInfo
accountInfo user = sg $ do
  renderAccountInfo user
