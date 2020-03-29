module Le.Time where

import Le.Import

type TimeZoneName = Text

type TimeZoneUILabel = Text

day :: NominalDiffTime
day = 60 * 60 * 24

hour :: NominalDiffTime
hour = 60 * 60
