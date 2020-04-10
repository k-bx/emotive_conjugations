module Le.App where

import qualified Data.String.Class as S
import qualified Database.Persist.Postgresql as P
import qualified Dhall
import GHC.Conc (numCapabilities)
import Le.Import
import qualified Network.AWS as AWS
import qualified Network.HTTP.Client
import qualified RIO.Process
import qualified System.Directory

withApp :: (App -> IO a) -> IO a
withApp f = do
  cfg <- liftIO readConfig
  lo <-
    logOptionsHandle stderr False
      <&> setLogUseTime True
  pc <- RIO.Process.mkDefaultProcessContext
  httpManager <- Network.HTTP.Client.newManager Network.HTTP.Client.defaultManagerSettings
  httpManagerNoTimeout <- Network.HTTP.Client.newManager (Network.HTTP.Client.defaultManagerSettings {Network.HTTP.Client.managerResponseTimeout = Network.HTTP.Client.responseTimeoutNone})
  httpManagerPython <- Network.HTTP.Client.newManager (Network.HTTP.Client.defaultManagerSettings {Network.HTTP.Client.managerResponseTimeout = Network.HTTP.Client.responseTimeoutNone})
  let dataDir = cfgDataDir cfg
  liftIO $ System.Directory.createDirectoryIfMissing True dataDir
  withLogFunc lo $ \lf -> do
    awsEnv <- AWS.newEnv (AWS.FromFile "conj" "sysadmin/aws_credentials")
    -- runNoLoggingT $
    P.withPostgresqlPool (S.fromText (cfgPsqlConnString cfg)) 5
      $ \pool -> liftIO $ do
        withSystemTempDirectory "conj-webapp" $ \tempDirPath -> do
          let app = App
                { appLogFunc = lf,
                  appProcessContext = pc,
                  appConfig = cfg,
                  appAwsEnv = awsEnv,
                  appTempDir = tempDirPath,
                  appHttpManager = httpManager,
                  appHttpManagerNoTimeout = httpManagerNoTimeout,
                  appHttpManagerPython = httpManagerPython,
                  appDataDir = dataDir,
                  appNumCapabilities = numCapabilities,
                  appDb = pool
                }
          runRIO app $ logInfo $ display $ "> tempDirPath: " <> S.toText tempDirPath
          f app

readConfig :: IO Config
readConfig = do
  h <- System.Directory.getHomeDirectory
  Dhall.input Dhall.auto (S.toText (h <> "/conj.dhall"))

aws :: AWS.AWS a -> Le a
aws action = do
  App {appAwsEnv} <- ask
  AWS.runResourceT $ AWS.runAWS appAwsEnv action

run :: RIO App a -> IO a
run act = withApp $ \env -> runRIO env act

runDb :: ReaderT P.SqlBackend IO b -> Le b
runDb f = do
  env <- ask
  liftIO $ flip P.runSqlPool (appDb env) $ f
