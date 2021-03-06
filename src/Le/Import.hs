{-# LANGUAGE NoImplicitPrelude #-}

module Le.Import
  ( module RIO,
    module X,
    ev,
    toBS,
    fromBS,
    fromLBS,
    toLBS,
    q,
    qc,
  )
where

import Control.Monad.Logger as X (NoLoggingT (..))
import Control.Monad.Trans.Resource as X
import Control.Newtype as X (Newtype (..))
import qualified Data.ByteString.Lazy as BL
import Data.Either.Validation as X (Validation (..))
import Data.Either.ValidationT as X
  ( ValidateT,
    bimapValidateT,
    runValidateT,
    throwVT,
  )
import Data.Monoid as X (Sum (..))
import qualified Data.String.Class as S
import Data.Time.Additional as X
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
import Language.Haskell.TH.Quote (QuasiQuoter)
import Le.Aeson as X (FromJSON (..), ToJSON (..), jsonOpts)
import Le.Config as X
import Le.Types as X
import NeatInterpolation as X (trimming, untrimming)
import Network.URI as X (URI (..), URIAuth (..))
import RIO
import Safe as X (fromJustNote)
import qualified Text.RawString.QQ

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

q :: QuasiQuoter
q = Text.RawString.QQ.r

qc :: QuasiQuoter
qc = trimming
