{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Le.Model where

import Data.Text as Text
import Database.Persist.Postgresql
import Database.Persist.TH
import qualified Le.ApiTypes.Modeled as AT
import Le.Import
import qualified Le.Python

-- | See Note [Indexes and Migrations]
share
  [mkPersist sqlSettings, mkDeleteCascade sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
User
    email Text
    createdAt UTCTime
    updatedAt UTCTime
    UniqueEmail email
    deriving Show
LoginToken
    tokenVal LoginTokenVal
    userId UserId
    LoginTokenQuery tokenVal
    createdAt UTCTime
    Primary tokenVal
    deriving Show
LoginCode
    code Text
    email Text
    createdAt UTCTime
    Primary code
    deriving Show
Article
    warcId Text Maybe
    warcDate UTCTime
    url Text
    host Text
-- -- | Article extracted via Python newspaper library.
-- -- Article.id == ArticleNp.id
-- ArticleNp
--     url Text -- duplicate for convenience
--     host Text -- duplicate for convenience
--     title Text
--     authors (JsonList Text)
--     date UTCTime Maybe
--     content Text
--     lang Text
--     spacyNer Le.Python.CmdSpacyNerRes Maybe
--     spacyPos Le.Python.CmdSpacyPosRes Maybe
--     deriving Show
-- | Article extracted via Python news-please library.
-- Article.id == ArticlePlease.id
ArticlePlease
    url Text -- duplicate for convenience
    host Text -- duplicate for convenience
    authors (JsonList Text)
    dateDownload UTCTime Maybe
    datePublish UTCTime Maybe
    dateModify UTCTime Maybe
    description Text
    filename Text
    imageUrl Text
    language Text
    localpath Text Maybe
    title Text Maybe
    titlePage Text Maybe
    titleRss Text Maybe
    sourceDomain Text Maybe
ArticlePleaseBig
    maintext Text
    spacyNer Le.Python.CmdSpacyNerRes Maybe
    spacyPos Le.Python.CmdSpacyPosRes Maybe
    deriving Show
NamedEntity
    articlePleaseId ArticlePleaseId
    entity Text
    start Int
    startChar Int
    end Int
    endChar Int
    label_ Text
    search1 Text Maybe
    search2 Text Maybe
    search3 Text Maybe
    canonical Text Maybe
    proper Text Maybe
    deriving Show
-- | Cache for named entity propers
NamedPropers
    entity Text
    proper Text
    Primary entity
    -- UniqueNamedPropersEntity entity
Queue
    userId UserId
    url Text
    status AT.QueueItemStatus
    createdAt UTCTime
    updatedAt UTCTime
--
-- Migration data. Singleton object
--
MigrationInfo
  version Int
|]
