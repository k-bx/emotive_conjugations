module Le.Types where

import qualified Data.String.Class as S
import qualified Dhall
import qualified Network.AWS as AWS
import qualified Network.AWS
import RIO
import RIO.Process
import qualified System.Directory

type AppM = RIO App

-- | Command line arguments
data Config
  = Config
      { cfgHttpPort :: Maybe Natural
      }
  deriving (Generic)

instance Dhall.FromDhall Config

data App
  = App
      { appLogFunc :: !LogFunc,
        appProcessContext :: !ProcessContext,
        appConfig :: !Config,
        appAwsEnv :: Network.AWS.Env
        -- Add other app-specific configuration information here
      }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})

withApp :: (App -> IO a) -> IO a
withApp f = do
  cfg <- liftIO readConfig
  lo <- logOptionsHandle stderr False
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf -> do
    -- awsEnv <- AWS.newEnv AWS.Discover
    awsEnv <- AWS.newEnv (AWS.FromFile "conj" "sysadmin/aws_credentials")
    let app = App
          { appLogFunc = lf,
            appProcessContext = pc,
            appConfig = cfg,
            appAwsEnv = awsEnv
          }
    f app

readConfig :: IO Config
readConfig = do
  h <- System.Directory.getHomeDirectory
  Dhall.input Dhall.auto (S.toText (h <> "/conj.dhall"))
