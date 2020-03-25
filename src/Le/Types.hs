module Le.Types where

import qualified Dhall
import qualified Network.AWS
import RIO
import RIO.Process

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
