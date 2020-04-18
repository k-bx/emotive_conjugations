{-# LANGUAGE TemplateHaskell #-}

module Le.ApiTypes where

import qualified Data.Aeson as J
import Elm.Derive
import Le.Import
import qualified Le.Model as M
import qualified Le.Python

data NoOp = NoOp
  { nooField :: Text
  }
  deriving (Show, Eq, Generic)

deriveBoth (jsonOpts 3) ''NoOp

type IntZonedTime = Int

data DownloadAndFilterForm = DownloadAndFilterForm
  { dafWarcFile :: Text
  }
  deriving (Show, Eq, Generic)

instance J.ToJSON DownloadAndFilterForm where
  toEncoding = J.genericToEncoding (jsonOpts 3)

  toJSON = J.genericToJSON (jsonOpts 3)

instance J.FromJSON DownloadAndFilterForm where
  parseJSON = J.genericParseJSON (jsonOpts 3)

data ArticleShort = ArticleShort
  { artId :: M.ArticleId,
    artDate :: Maybe IntZonedTime,
    artPaperName :: Text,
    artTitleShort :: Text
  }
  deriving (Show, Eq, Generic)

deriveBoth (jsonOpts 3) ''ArticleShort

data Article = Article
  { arcId :: M.ArticleId,
    arcUrl :: Text,
    arcDate :: Maybe IntZonedTime,
    arcPaperName :: Text,
    arcTitle :: Text,
    arcAuthors :: [Text],
    arcLang :: Text
  }
  deriving (Show, Eq, Generic)

deriveBoth (jsonOpts 3) ''Article

-- data ArticleNp
--   = ArticleNp
--       { arnId :: M.ArticleNpId,
--         arnAuthors :: [Text],
--         arnDate :: Maybe IntZonedTime,
--         arnContent :: Text,
--         arnLang :: Text,
--         arnSpacyNerEnts :: Maybe [Le.Python.CmdSpacyNerResEnt],
--         arnSpacyPosEnts :: Maybe [Le.Python.CmdSpacyPosResEnt]
--       }
--   deriving (Show, Eq, Generic)

-- deriveBoth (jsonOpts 3) ''ArticleNp

data ArticlePlease = ArticlePlease
  { arpId :: M.ArticlePleaseId,
    arpAuthors :: [Text],
    arpDatePublish :: Maybe IntZonedTime,
    -- arpMaintext :: Text,
    arpLanguage :: Maybe Text
    -- arpSpacyNerEnts :: Maybe [Le.Python.CmdSpacyNerResEnt],
    -- arpSpacyPosEnts :: Maybe [Le.Python.CmdSpacyPosResEnt]
  }
  deriving (Show, Eq, Generic)

deriveBoth (jsonOpts 3) ''ArticlePlease

data Paginated item = Paginated
  { pgnItems :: [item],
    pgnOverallPages :: Int
  }
  deriving (Show, Eq, Generic)

deriveBoth (jsonOpts 3) ''Paginated

data NamedEntityGroup = NamedEntityGroup
  { nerEntity :: Text,
    nerGroup :: [Text]
  }
  deriving (Show, Eq, Generic)

deriveBoth (jsonOpts 3) ''NamedEntityGroup
