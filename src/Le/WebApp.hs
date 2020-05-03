{-# LANGUAGE TypeOperators #-}

module Le.WebApp where

import Control.Monad.Trans.Except (ExceptT (..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Pool
import qualified Data.String.Class as S
import qualified Data.Text as T
import qualified Database.Persist.Postgresql as P
import GHC.Conc (numCapabilities)
import GHC.Stack
import qualified Le.App
import qualified Le.Config
import Le.Import
import Le.Model
import Le.Routes
import qualified Network.Wai as Wai
import qualified Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Gzip as Gzip
import qualified Network.Wai.Middleware.Static as MStatic
import Servant
import Servant.API.Generic (ToServantApi)
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import Servant.Server.Generic (genericServerT)
import qualified System.Directory
import qualified Web.Cookie as Cookie
import qualified Prelude

genAuthServerContext ::
  Data.Pool.Pool P.SqlBackend ->
  Context (AuthHandler Wai.Request (P.Entity User) ': '[])
genAuthServerContext db = (authHandler db) :. EmptyContext

lookupUserByToken ::
  MonadIO m =>
  UTCTime ->
  LoginTokenVal ->
  ReaderT P.SqlBackend m (Maybe (Entity User))
lookupUserByToken t tokenVal = do
  mToken <- P.getBy (LoginTokenQuery tokenVal)
  case mToken of
    Nothing -> return Nothing
    Just tok ->
      if addUTCTime Le.Config.tokenExpirationPeriod (loginTokenCreatedAt (entityVal tok))
        > t
        then P.getEntity (loginTokenUserId (entityVal tok))
        else return Nothing

lookupAccount ::
  Network.Wai.Request ->
  Data.Pool.Pool P.SqlBackend ->
  LoginTokenVal ->
  Servant.Handler (P.Entity User)
lookupAccount req db tok = do
  t <- liftIO $ getCurrentTime
  mToken <- liftIO $ flip P.runSqlPool db $ lookupUserByToken t tok
  case mToken of
    Nothing ->
      errorOutOrGotoLogin req "Invalid Token"
    Just user -> return user

authHandler :: Data.Pool.Pool P.SqlBackend -> AuthHandler Wai.Request (P.Entity User)
authHandler db = mkAuthHandler handler
  where
    maybeToEither e = maybe (Left e) Right
    handler req = do
      let eCookie =
            maybeToEither "Missing cookie header"
              $ lookup "cookie"
              $ Network.Wai.requestHeaders req
      case eCookie of
        Left e -> errorOutOrGotoLogin req e
        Right cookie -> do
          case lookup "u" (Cookie.parseCookies cookie) of
            Nothing -> errorOutOrGotoLogin req "Missing token in cookie"
            Just u -> do
              lookupAccount req db (pack (S.toText u))

errorOutOrGotoLogin :: Network.Wai.Request -> BL.ByteString -> Servant.Handler (Entity User)
errorOutOrGotoLogin req msg = do
  case lookup "Accept" (Network.Wai.requestHeaders req) of
    Nothing -> throw401 msg
    Just bs ->
      case "text/html" `B.isInfixOf` bs of
        True -> throwError $ err302 {errHeaders = [("Location", "/login")]}
        False -> throw401 msg
  where
    throw401 msg' = throwError $ err401 {errBody = msg'}

nt :: Env -> RIO Env a -> Servant.Handler a
nt env action = Servant.Handler $ ExceptT $ try $ runRIO env action

onExceptionAct :: HasCallStack => Maybe Wai.Request -> SomeException -> IO ()
onExceptionAct mReq exc = do
  let url =
        case mReq of
          Nothing -> ""
          Just req -> S.toText (Wai.rawPathInfo req)
  S.putStrLn
    ( "Error: caught an exception Path: " <> url <> " Error: " <> tshow exc
        <> ". Call stack: "
        <> S.toText (prettyCallStack callStack)
    )

run :: Text -> IO ()
run ver = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  Prelude.putStrLn $ "Starting webapp version " <> S.toString ver <> " at https://emotive-conjugations.localhost:8080/ ..."
  Prelude.putStrLn $ "Running on N cores: " ++ show numCapabilities
  dir <- System.Directory.canonicalizePath "."
  Prelude.putStrLn $ "> dir: " <> dir
  Le.App.withApp $ \env -> do
    let cfg = envConfig env
    Prelude.putStrLn $ "> working with db: " <> S.toString (T.take 50 (cfgPsqlConnString cfg) <> "...")
    cacheContainer <- liftIO $ MStatic.initCaching MStatic.PublicStaticCaching
    let staticOptions =
          MStatic.defaultOptions {MStatic.cacheContainer = cacheContainer}
        staticPolicy =
          MStatic.noDots <> MStatic.isNotAbsolute <> MStatic.addBase dir
        waiApp :: Wai.Application
        waiApp =
          Gzip.gzip
            (Gzip.def {Gzip.gzipFiles = Gzip.GzipCompress})
            ( MStatic.staticPolicyWithOptions
                staticOptions
                staticPolicy
                ( serveWithContext
                    (Proxy :: Proxy (ToServantApi API))
                    (genAuthServerContext (envDb env))
                    ( hoistServerWithContext
                        (Proxy :: Proxy (ToServantApi API))
                        (Proxy :: Proxy '[AuthHandler Wai.Request (P.Entity User)])
                        (nt env)
                        (genericServerT server)
                    )
                )
            )
    -- let tlsSettings =
    --       WarpTLS.tlsSettings
    --         (S.toString (cfgCertPath cfg))
    --         (S.toString (cfgKeyPath cfg))
    -- let settingsHttps =
    --       Warp.setOnException onExceptionAct $
    --         Warp.setPort (cfgHttpsPort cfg) Warp.defaultSettings
    -- let runHttps :: IO ()
    --     runHttps = WarpTLS.runTLS tlsSettings settingsHttps waiApp
    let httpPort = fromIntegral (fromMaybe (error "Port must be set in config!") (cfgHttpPort cfg))
    let runHttp :: IO ()
        runHttp = Warp.run httpPort waiApp
    runHttp
