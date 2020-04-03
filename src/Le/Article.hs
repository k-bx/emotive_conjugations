module Le.Article where

import qualified Data.String.Class as S
import Le.Import
import Le.Util
import qualified Network.URI

extractHost :: Text -> Maybe Text
extractHost uriText =
  uriText
    |> S.toString
    |> Network.URI.parseURI
    |> fmap uriAuthority
    |> join
    |> fmap uriRegName
    |> fmap S.toText

extractHostUnsafe =
  fromJustNote "impossible! couldn't parse url host" . extractHost
