module Data.Time.Additional where

import Control.Concurrent (threadDelay)
import qualified Data.Fixed
import Data.Time
import qualified Data.Time.Clock as Time
import qualified Data.Time.Clock.POSIX as Time
import qualified Data.Time.Zones
import Prelude

nominalDiffTimeToMilliseconds :: Time.NominalDiffTime -> Int
nominalDiffTimeToMilliseconds t =
  let Data.Fixed.MkFixed s = nominalDiffTimeToSeconds t
   in fromIntegral (s `div` ((10 :: Integer) ^ (9 :: Int)))

utcTimeToMilliseconds :: Time.UTCTime -> Int
utcTimeToMilliseconds =
  nominalDiffTimeToMilliseconds . Time.utcTimeToPOSIXSeconds

-- | Erases tz info without time arithmetic.
-- See Note [TimeZone and Wire]
zonedTimeToMilliseconds :: ZonedTime -> Int
zonedTimeToMilliseconds (ZonedTime localTime _tz) =
  utcTimeToMilliseconds (localTimeToUTC utc localTime)

-- | See Note [TimeZone Conversion]
utcToZonedTime' :: Data.Time.Zones.TZ -> UTCTime -> ZonedTime
utcToZonedTime' tz t =
  utcToZonedTime (Data.Time.Zones.timeZoneForUTCTime tz t) t

-- | Format taken by threadDelay and friends
nominalDiffToMicroSeconds :: Integral a => NominalDiffTime -> a
nominalDiffToMicroSeconds x = truncate ((realToFrac x :: Double) * 1000000)

threadDelaySecs :: NominalDiffTime -> IO ()
threadDelaySecs n = threadDelay (nominalDiffToMicroSeconds n)
