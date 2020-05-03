module Le.App where

import Conduit ((.|))
import qualified Conduit as C
import qualified Data.String.Class as S
import qualified Database.Persist.Postgresql as P
import qualified Dhall
import GHC.Conc (numCapabilities)
import Le.Import
import qualified Network.AWS as AWS
import qualified Network.HTTP.Client
import qualified Network.HTTP.Client.TLS
import qualified RIO.Process
import qualified System.Directory

withApp :: (Env -> IO a) -> IO a
withApp f = do
  cfg <- liftIO readConfig
  lo <-
    logOptionsHandle stderr False
      <&> setLogUseTime True
  pc <- RIO.Process.mkDefaultProcessContext
  httpManager <- Network.HTTP.Client.TLS.newTlsManagerWith Network.HTTP.Client.TLS.tlsManagerSettings
  httpManagerNoTimeout <- Network.HTTP.Client.TLS.newTlsManagerWith (Network.HTTP.Client.TLS.tlsManagerSettings {Network.HTTP.Client.managerResponseTimeout = Network.HTTP.Client.responseTimeoutNone})
  httpManagerPython <- Network.HTTP.Client.newManager (Network.HTTP.Client.defaultManagerSettings {Network.HTTP.Client.managerResponseTimeout = Network.HTTP.Client.responseTimeoutNone})
  let dataDir = cfgDataDir cfg
  liftIO $ System.Directory.createDirectoryIfMissing True dataDir
  withLogFunc lo $ \lf -> do
    awsEnv <- AWS.newEnv (AWS.FromFile "conj" "sysadmin/aws_credentials")
    runNoLoggingT $ P.withPostgresqlPool (S.fromText (cfgPsqlConnString cfg)) 5 $ \pool -> do
      runNoLoggingT $ P.withPostgresqlPool (S.fromText (cfgRemotePsqlConnString cfg)) 5 $ \poolRemote -> liftIO $ do
        withSystemTempDirectory "conj-webapp" $ \tempDirPath -> do
          let env =
                Env
                  { envLogFunc = lf,
                    envProcessContext = pc,
                    envConfig = cfg,
                    envAwsEnv = awsEnv,
                    envTempDir = tempDirPath,
                    envHttpManager = httpManager,
                    envHttpManagerNoTimeout = httpManagerNoTimeout,
                    envHttpManagerPython = httpManagerPython,
                    envDataDir = dataDir,
                    envNumCapabilities = numCapabilities,
                    envDb = pool,
                    envDbRemote = poolRemote
                  }
          runRIO env $ logInfo $ display $ "> tempDirPath: " <> S.toText tempDirPath
          f env

readConfig :: IO Config
readConfig = do
  h <- System.Directory.getHomeDirectory
  Dhall.input Dhall.auto (S.toText (h <> "/conj.dhall"))

aws :: AWS.AWS a -> Le a
aws action = do
  Env {envAwsEnv} <- ask
  AWS.runResourceT $ AWS.runAWS envAwsEnv action

run :: Le a -> IO a
run act = withApp $ \env -> runRIO env act

runDb :: ReaderT P.SqlBackend IO b -> Le b
runDb f = do
  env <- ask
  liftIO $ flip P.runSqlPool (envDb env) $ f

-- | Run in a remote "queue" db
runDbRemote :: ReaderT P.SqlBackend IO b -> Le b
runDbRemote f = do
  env <- ask
  liftIO $ flip P.runSqlPool (envDbRemote env) $ f

forCondRes ::
  MonadUnliftIO m =>
  C.Acquire (C.ConduitM () b m ()) ->
  (b -> m ()) ->
  m ()
forCondRes res act =
  C.withAcquire res $ \src ->
    C.runConduit $
      src .| C.mapM_C act

forCondResEnum ::
  (MonadUnliftIO m, Num a1, Enum a1) =>
  C.Acquire (C.ConduitT () a2 m ()) ->
  ((a1, a2) -> m ()) ->
  m ()
forCondResEnum res act =
  C.withAcquire res $ \src ->
    C.runConduit $
      (C.getZipSource ((,) <$> C.ZipSource (C.yieldMany [0 ..]) <*> C.ZipSource src))
        .| C.mapM_C act

askLogI ::
  (MonadUnliftIO m2, MonadIO m, MonadReader Env m2) =>
  m2 (Text -> m ())
askLogI = askRunInIO & fmap (\runInIO -> \t -> liftIO $ runInIO $ logInfo $ display t)
