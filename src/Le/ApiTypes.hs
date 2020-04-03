{-# LANGUAGE TemplateHaskell #-}

module Le.ApiTypes where

import qualified Data.Aeson as J
import Elm.Derive
import Le.Import

data NoOp
  = NoOp
      { nooField :: Text
      }
  deriving (Show, Eq, Generic)

deriveBoth (jsonOpts 3) ''NoOp

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
