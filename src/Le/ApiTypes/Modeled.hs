{-# LANGUAGE TemplateHaskell #-}

module Le.ApiTypes.Modeled where

import qualified Data.Text as T
import qualified Database.Persist.Postgresql as P
import Elm.Derive
import Le.AppUtils
import Le.Import

data QueueItemStatus
  = QueueItemStatusQueued
  | QueueItemStatusDownloading
  | QueueItemStatusExtracting
  | QueueItemStatusNer
  | QueueItemStatusPos
  | QueueItemStatusDone
  deriving (Show, Eq, Generic)

deriveBoth (jsonOpts (T.length "QueueItemStatus")) ''QueueItemStatus

instance P.PersistFieldSql QueueItemStatus where
  sqlType _ = P.SqlString

instance P.PersistField QueueItemStatus where
  toPersistValue = toPersistValueJSONText
  fromPersistValue = fromPersistValueJSONText
