module Le.Article where

import qualified Data.String.Class as S
import qualified Data.Text as T
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

extractHostUnsafe :: Text -> Text
extractHostUnsafe =
  fromJustNote "impossible! couldn't parse url host" . extractHost

newspaperNameFromHost :: Text -> Text
newspaperNameFromHost t =
  let pairs :: [(Text, Text)]
      pairs =
        [ ("nytimes.com", "The New York Times"),
          ("bbc.com", "BBC"),
          ("www.bbc.co.uk", "BBC"),
          ("foxnews.com", "Fox News Channel"),
          ("washingtonpost.com", "The Washington Post"),
          ("cnn.com", "CNN")
        ]
   in pairs
        |> map
          ( \(domain, name) ->
              case t `T.isInfixOf` domain || (t `T.isInfixOf` ("www." <> domain)) of
                False -> Nothing
                True -> Just name
          )
        |> catMaybes
        |> listToMaybe
        |> fromMaybe t
