module Le.Speed where

import qualified Data.Text as T
import Data.Time
import Le.Import

reportDelay :: NominalDiffTime
reportDelay = 5

data Speed = Speed
  { spdStarted :: UTCTime,
    spdOverall :: Int,
    spdLastReport :: TVar UTCTime,
    spdLastRecord :: TVar Int
  }

newSpeed :: MonadIO m => Int -> m Speed
newSpeed overall = do
  t <- liftIO $ getCurrentTime
  lrep <- atomically $ newTVar t
  lrec <- atomically $ newTVar 0
  pure $ Speed t overall lrep lrec

withProgress :: MonadIO m => Int -> Speed -> (Text -> m ()) -> m ()
withProgress curr Speed {..} f = do
  lastReport <- atomically $ readTVar spdLastReport
  let noReportTillThisMark = addUTCTime reportDelay lastReport
  t <- liftIO getCurrentTime
  when (t > noReportTillThisMark) $ do
    lastRecord <- readTVarIO (spdLastRecord)
    atomically $ do
      writeTVar spdLastReport t
      writeTVar spdLastRecord curr
    let timePassedSinceLastTime = Data.Time.diffUTCTime t lastReport
        timeLeftGlobal :: NominalDiffTime
        timeLeftGlobal =
          case curr of
            0 -> 1000000000
            _ ->
              (fromIntegral ((spdOverall - curr) `div` curr))
                * (Data.Time.diffUTCTime t spdStarted)
        recsPerSecond :: Int
        recsPerSecond =
          case round (nominalDiffTimeToSeconds timePassedSinceLastTime) of
            0 -> 0
            timeSinceLast ->
              (curr - lastRecord) `div` timeSinceLast
    let res =
          T.concat
            [ "[",
              tshow curr,
              " / ",
              tshow spdOverall,
              "] ",
              tshow recsPerSecond,
              " recs/second ",
              "~",
              tshow
                ( (round (nominalDiffTimeToSeconds timeLeftGlobal) :: Int)
                    `div` 60
                ),
              " minutes left"
            ]
    f res
