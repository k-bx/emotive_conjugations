module Le.Config where

import qualified Data.Time.Zones
import qualified Data.Time.Zones.All
import Le.Import
import Servant.Client

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
