{-# OPTIONS_GHC -fno-warn-orphans #-}

module Le.Types where

import Control.Monad.Logger (MonadLogger (..), toLogStr)
import qualified Data.String.Class as S
import qualified Dhall
import qualified Network.AWS
import qualified Network.HTTP.Client
import RIO
import RIO.Process
import System.Log.FastLogger (fromLogStr)

type AppM = RIO App

type Le = RIO App

-- | Command line arguments
data Config
  = Config
      { cfgHttpPort :: Maybe Natural,
        cfgMode :: Mode,
        cfgDataDir :: FilePath,
        cfgPsqlConnString :: Text
      }
  deriving (Generic)

instance Dhall.FromDhall Config

data Mode = Master | Worker
  deriving (Generic)

instance Dhall.FromDhall Mode

data App
  = App
      { appLogFunc :: !LogFunc,
        appProcessContext :: !ProcessContext,
        appConfig :: !Config,
        appAwsEnv :: Network.AWS.Env,
        appTempDir :: FilePath,
        appHttpManager :: Network.HTTP.Client.Manager,
        appHttpManagerNoTimeout :: Network.HTTP.Client.Manager,
        appDataDir :: FilePath,
        appNumCapabilities :: Int
      }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})

-- | 'P.withPostgresqlPool' needs this.
instance MonadLogger IO where
  monadLoggerLog _loc _logSource _logLevel msg =
    liftIO (S.hPutStrLn stderr (fromLogStr (toLogStr msg)))
