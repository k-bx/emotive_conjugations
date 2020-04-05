{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Le.Model where

import Data.Text as Text
import Database.Persist.Postgresql
import Database.Persist.TH
import Le.Import

-- | See Note [Indexes and Migrations]
share
  [mkPersist sqlSettings, mkDeleteCascade sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
User
    email Text
    deriving Show
Article
    url Text -- duplicate for convenience
    host Text -- duplicate for convenience
-- | Article extracted via Python newspaper library.
-- Article.id == ANewspaper.id
ANewspaper
    url Text -- duplicate for convenience
    host Text -- duplicate for convenience
    title Text
    authors (JsonList Text)
    date UTCTime Maybe
    content Text
    lang Text
    deriving Show
--
-- Migration data. Singleton object
--
MigrationInfo
  version Int
|]
