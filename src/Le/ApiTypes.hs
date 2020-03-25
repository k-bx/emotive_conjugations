module Le.ApiTypes where

import qualified Data.Aeson as J
import Le.Import

data DownloadAndFilterForm
  = DownloadAndFilterForm
      { dafWarcFile :: Text
      }
  deriving (Show, Eq, Generic)

instance J.ToJSON DownloadAndFilterForm where

  toEncoding = J.genericToEncoding (jsonOpts 3)

  toJSON = J.genericToJSON (jsonOpts 3)

instance J.FromJSON DownloadAndFilterForm where
  parseJSON = J.genericParseJSON (jsonOpts 3)
