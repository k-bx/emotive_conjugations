module Le.Config where

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
    "bbc.co.uk",
    "dailymail.co.uk"
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
