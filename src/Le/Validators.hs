module Le.Validators where

import qualified Data.String.Class as S
import qualified Data.Text as T
import qualified Data.Time.Zones.All
import qualified Database.Persist.Postgresql as P
import Le.Import
import qualified Le.Time
import Le.Types
import Network.URI as URI

parseUrl :: Applicative m => Text -> ValidateT Text m PersistentAbsoluteURI
parseUrl txt =
  case URI.parseAbsoluteURI (S.toString txt) of
    Nothing -> throwVT $ "Not a valid url: " <> txt
    Just u -> pure (pack u)
