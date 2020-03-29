module Le.WebApp where

import Control.Monad.Trans.Except (ExceptT (..))
import qualified Data.String.Class as S
import GHC.Conc (numCapabilities)
import GHC.Stack
import qualified Le.App
import Le.Import
import Le.Routes
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Gzip as Gzip
import qualified Network.Wai.Middleware.Static as MStatic
import Servant
import Servant.API.Generic (ToServantApi)
import Servant.Server.Generic (genericServerT)
import qualified System.Directory
import qualified Prelude

-- genAuthServerContext ::
--   Pool P.SqlBackend ->
--   Context (AuthHandler Wai.Request (P.Entity User) ': '[])
-- genAuthServerContext db = (authHandler db) :. EmptyContext

nt :: App -> RIO App a -> Servant.Handler a
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

run :: IO ()
run = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  Prelude.putStrLn "Starting webapp at https://salesrt.localhost:8081/ ..."
  Prelude.putStrLn $ "Running on N cores: " ++ show numCapabilities
  dir <- System.Directory.canonicalizePath "."
  Prelude.putStrLn $ "> dir: " <> dir
  Le.App.withApp $ \app -> do
    let cfg = appConfig app
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
                    -- (genAuthServerContext (appDb env))
                    EmptyContext
                    ( hoistServerWithContext
                        (Proxy :: Proxy (ToServantApi API))
                        -- (Proxy :: Proxy '[AuthHandler Wai.Request (P.Entity User)])
                        (Proxy :: Proxy '[])
                        (nt app)
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
