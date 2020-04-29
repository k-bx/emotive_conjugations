module Le.Time where

import qualified Le.Config
import Le.Import

type TimeZoneName = Text

type TimeZoneUILabel = Text

day :: NominalDiffTime
day = 60 * 60 * 24

hour :: NominalDiffTime
hour = 60 * 60

renderTime :: UTCTime -> Int
renderTime = zonedTimeToMilliseconds . utcToZonedTime' Le.Config.tz
