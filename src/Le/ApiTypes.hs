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

type IntZonedTime = Int

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

data ArticleShort
  = ArticleShort
      { artDate :: IntZonedTime,
        artPaperName :: Text,
        artTitleShort :: Text
      }
  deriving (Show, Eq, Generic)

deriveBoth (jsonOpts 3) ''ArticleShort
