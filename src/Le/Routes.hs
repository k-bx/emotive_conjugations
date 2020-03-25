{-# LANGUAGE TypeOperators #-}

module Le.Routes where

import Le.Handlers
import Le.Import
import Servant
import Servant.API.Generic
import Servant.Server.Generic

data API route
  = API
      { __ping :: route :- "api" :> "ping" :> Get '[PlainText] Text,
        __jsonApi :: route :- ToServantApi JsonAPI
      }
  deriving (Generic)

data JsonAPI route
  = JsonAPI
      { _pingJson ::
          route
            :- "api" :> "ping.json" :> Get '[JSON] [Text],
        _errorOut ::
          route
            :- "api" :> "error-out.json" :> Get '[JSON] [Text]
      }
  deriving (Generic)

server :: API (AsServerT (RIO App))
server =
  API
    { __ping = ping,
      __jsonApi = toServant jsonApi
    }
  where
    jsonApi :: JsonAPI (AsServerT (RIO App))
    jsonApi =
      JsonAPI
        { _pingJson = pingJson,
          _errorOut = errorOut
        }
