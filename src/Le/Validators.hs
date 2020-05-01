module Le.Validators where

import qualified Data.String.Class as S
import Le.Import
import Network.URI as URI

parseUrl :: Applicative m => Text -> ValidateT Text m PersistentAbsoluteURI
parseUrl txt =
  case URI.parseAbsoluteURI (S.toString txt) of
    Nothing -> throwVT $ "Not a valid url: " <> txt
    Just u -> pure (pack u)
