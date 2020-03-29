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
Article
    url Text
    host Text
    content Text
    date UTCTime
    deriving Show
--
-- Migration data. Singleton object
--
MigrationInfo
  version Int
|]
