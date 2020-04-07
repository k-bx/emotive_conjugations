{-# OPTIONS_GHC -fno-warn-orphans #-}

module Le.Types where

import Control.Monad.Logger (MonadLogger (..), toLogStr)
import Control.Newtype
import qualified Data.Aeson as J
import Data.Pool
import qualified Data.String.Class as S
import qualified Database.Persist.Postgresql as P
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
        cfgPsqlConnString :: Text,
        cfgPythonWebapp :: Text
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
        -- | local python flask worker (scripts/webserver.py)
        appHttpManagerPython :: Network.HTTP.Client.Manager,
        appDataDir :: FilePath,
        appNumCapabilities :: Int,
        appDb :: Pool P.SqlBackend
      }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})

-- | 'P.withPostgresqlPool' needs this.
instance MonadLogger IO where
  monadLoggerLog _loc _logSource _logLevel msg =
    liftIO (S.hPutStrLn stderr (fromLogStr (toLogStr msg)))

newtype JsonList a
  = JsonList [a]
  deriving (Show)

unJsonList :: JsonList a -> [a]
unJsonList (JsonList xs) = xs

instance (J.ToJSON a, J.FromJSON a) => P.PersistField (JsonList a) where

  toPersistValue = P.toPersistValueJSON . unJsonList

  fromPersistValue x =
    case x of
      P.PersistText "" -> Right (JsonList [])
      _ -> fmap JsonList (P.fromPersistValueJSON x)

instance (J.ToJSON a, J.FromJSON a) => P.PersistFieldSql (JsonList a) where
  sqlType _ = P.SqlString -- sqlUtf8Text

instance Newtype (JsonList a) [a] where

  pack = JsonList

  unpack (JsonList x) = x
