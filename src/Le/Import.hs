{-# LANGUAGE NoImplicitPrelude #-}

module Le.Import
  ( module RIO,
    module Le.Types,
    module X,
    ev,
    toBS,
    fromBS,
    fromLBS,
    toLBS,
  )
where

import Control.Monad.Logger as X (NoLoggingT (..))
import Control.Monad.Trans.Resource as X
import Control.Newtype as X (Newtype (..))
import qualified Data.ByteString.Lazy as BL
import Data.Monoid as X (Sum (..))
import qualified Data.String.Class as S
import Data.Time.Clock as X
  ( NominalDiffTime,
    UTCTime,
    addUTCTime,
    getCurrentTime,
  )
import Data.Time.Clock.POSIX as X
  ( POSIXTime,
    posixSecondsToUTCTime,
    utcTimeToPOSIXSeconds,
  )
import Data.Time.LocalTime as X
  ( LocalTime,
    ZonedTime (..),
    zonedTimeToLocalTime,
    zonedTimeToUTC,
    zonedTimeZone,
  )
import Database.Persist.Postgresql as X (Entity (..), entityKey, entityVal)
import Le.Aeson as X (FromJSON (..), ToJSON (..), jsonOpts)
import Le.Types
import Network.URI as X (URI (..), URIAuth (..))
import RIO
import Safe as X (fromJustNote)

ev :: Entity record -> record
ev = entityVal

toBS :: S.ConvStrictByteString s => s -> ByteString
toBS = S.toStrictByteString

fromBS :: S.ConvStrictByteString s => ByteString -> s
fromBS = S.fromStrictByteString

toLBS :: S.ConvLazyByteString s => s -> BL.ByteString
toLBS = S.toLazyByteString

fromLBS :: S.ConvLazyByteString s => BL.ByteString -> s
fromLBS = S.fromLazyByteString
