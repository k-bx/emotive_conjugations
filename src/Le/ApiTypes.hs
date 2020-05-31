{-# LANGUAGE TemplateHaskell #-}

module Le.ApiTypes where

import qualified Data.Aeson as J
import Elm.Derive
import Le.ApiTypes.Modeled
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
    arcLang :: Text,
    arcWarcId :: Maybe Text
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
    arpDateDownload :: Maybe IntZonedTime,
    arpDatePublish :: Maybe IntZonedTime,
    arpDateModify :: Maybe IntZonedTime,
    arpLanguage :: Maybe Text
  }
  deriving (Show, Eq, Generic)

deriveBoth (jsonOpts 3) ''ArticlePlease

data ArticlePleaseBig = ArticlePleaseBig
  { arbId :: M.ArticlePleaseBigId,
    arbMaintext :: Text,
    arbSpacyNerEnts :: Maybe [Le.Python.CmdSpacyNerResEnt],
    arbTitleSpacyNerEnts :: Maybe [Le.Python.CmdSpacyNerResEnt],
    arbSpacyPosEnts :: Maybe [Le.Python.SpacyToken],
    arbTitleSpacyPosEnts :: Maybe [Le.Python.SpacyToken]
  }
  deriving (Show, Eq, Generic)

deriveBoth (jsonOpts 3) ''ArticlePleaseBig

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

data QueueAddForm = QueueAddForm
  { qafUrl :: Text
  }
  deriving (Show, Eq, Generic)

deriveBoth (jsonOpts 3) ''QueueAddForm

data QueueItem = QueueItem
  { quiId :: M.QueueId,
    quiUserId :: M.UserId,
    quiUrl :: Text,
    quiErrored :: Bool,
    quiStatus :: QueueItemStatus,
    quiArticleId :: Maybe M.ArticleId,
    quiCreatedAt :: IntZonedTime,
    quiUpdatedAt :: IntZonedTime
  }
  deriving (Show, Eq, Generic)

deriveBoth (jsonOpts 3) ''QueueItem

data AccountInfo = AccountInfo
  { accId :: M.UserId,
    accEmail :: Text
  }
  deriving (Show, Eq, Generic)

deriveBoth (jsonOpts 3) ''AccountInfo

data LogInSendPasswordForm = LogInSendPasswordForm
  { lisEmail :: Text
  }
  deriving (Show, Eq, Generic)

deriveBoth (jsonOpts 3) ''LogInSendPasswordForm

data LogInSendCodeForm = LogInSendCodeForm
  { licEmail :: Text,
    licCode :: Text
  }
  deriving (Show, Eq, Generic)

deriveBoth (jsonOpts 3) ''LogInSendCodeForm
