{-# OPTIONS_GHC -fno-warn-orphans #-}

module Le.Config where

import Control.Monad.Logger (MonadLogger (..), toLogStr)
import Data.Pool
import qualified Data.String.Class as S
import Data.Time.Clock
import qualified Data.Time.Zones
import qualified Data.Time.Zones.All
import qualified Database.Persist.Postgresql as P
import qualified Dhall
import qualified Network.AWS
import qualified Network.HTTP.Client
import RIO
import RIO.Process
import Servant.Client
import System.Log.FastLogger (fromLogStr)

type AppM = RIO Env

type Le = RIO Env

instance MonadFail (RIO Env) where
  fail = liftIO . fail

-- | Command line arguments
data Config = Config
  { cfgHttpPort :: Maybe Natural,
    cfgMode :: Mode,
    cfgDataDir :: FilePath,
    cfgPsqlConnString :: Text,
    cfgPythonWebapp :: Text,
    cfgMailgunDomain :: Text,
    cfgMailgunApiKey :: Text,
    cfgFacebookAppId :: Text,
    cfgFacebookAppSecret :: Text,
    -- cfgFacebookAppToken :: Text,
    cfgGoogleOauthClientId :: Text,
    cfgGoogleOauthClientSecret :: Text
  }
  deriving (Generic)

instance Dhall.FromDhall Config

data Mode = Master | Worker
  deriving (Generic)

instance Dhall.FromDhall Mode

data Env = Env
  { envLogFunc :: !LogFunc,
    envProcessContext :: !ProcessContext,
    envConfig :: !Config,
    envAwsEnv :: Network.AWS.Env,
    envTempDir :: FilePath,
    envHttpManager :: Network.HTTP.Client.Manager,
    envHttpManagerNoTimeout :: Network.HTTP.Client.Manager,
    -- | local python flask worker (scripts/webserver.py)
    envHttpManagerPython :: Network.HTTP.Client.Manager,
    envDataDir :: FilePath,
    envNumCapabilities :: Int,
    envDb :: Pool P.SqlBackend
  }

instance HasLogFunc Env where
  logFuncL = lens envLogFunc (\x y -> x {envLogFunc = y})

instance HasProcessContext Env where
  processContextL = lens envProcessContext (\x y -> x {envProcessContext = y})

-- | 'P.withPostgresqlPool' needs this.
instance MonadLogger IO where
  monadLoggerLog _loc _logSource _logLevel msg =
    liftIO (S.hPutStrLn stderr (fromLogStr (toLogStr msg)))

confEnvVarName :: String
confEnvVarName = "CONJ_CONFIG"

newsHosts :: [Text]
newsHosts =
  [ "nytimes.com",
    "cnn.com",
    "washingtonpost.com",
    "msnbc.com",
    "foxnews.com",
    "bbc.com",
    "bbc.co.uk"
    -- "dailymail.co.uk"
  ]

cheapWorkers :: [BaseUrl]
cheapWorkers =
  [ BaseUrl
      { baseUrlScheme = Http,
        baseUrlHost = "localhost",
        baseUrlPort = 6667,
        baseUrlPath = ""
      },
    BaseUrl
      { baseUrlScheme = Http,
        baseUrlHost = "localhost",
        baseUrlPort = 6668,
        baseUrlPath = ""
      }
  ]

-- | How many conj-webapp-python.service workers essentially
numPythonWorkers :: Int
numPythonWorkers = 4

-- | Data where CommonCrawl WARCs filtered for interesting news sites
-- is located
filteredDataDir :: Config -> FilePath
filteredDataDir cfg = cfgDataDir cfg <> "/filtered"

pythonScriptsDir :: FilePath
pythonScriptsDir =
  "/home/kb/workspace/emotive_conjugations/scripts"

pythonPath :: FilePath
pythonPath =
  "/home/kb/workspace/emotive_conjugations/venv/bin/python"

articlesLimit :: Int
articlesLimit = 100

entitiesPerPage :: Int
entitiesPerPage = 20

-- | For now we hardcode the usage of this timezone everywhere, in
-- future we can put it in company's settings or even accept in API if
-- needed.
tzLabel :: Data.Time.Zones.All.TZLabel
tzLabel = Data.Time.Zones.All.America__Chicago

tz :: Data.Time.Zones.TZ
tz = Data.Time.Zones.All.tzByLabel tzLabel

tokenExpirationPeriod :: NominalDiffTime
tokenExpirationPeriod = 60 * 60 * 24 * 60

loginCodeExpirationPeriod :: NominalDiffTime
loginCodeExpirationPeriod = 60 * 10

projectEmail :: Text
projectEmail = "emotive-conjugations@outlook.com"

fbLoginCallbackAddr :: Text
fbLoginCallbackAddr = "https://emotive-conjugations.app/api/fb-login-callback"

googleLoginCallbackAddr :: Text
googleLoginCallbackAddr = "https://emotive-conjugations.app/api/google-login-callback"
