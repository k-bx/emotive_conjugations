module Le.Speed where

import qualified Control.Concurrent
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Time
import Le.Import

reportDelay :: NominalDiffTime
reportDelay = 5

data Speed = Speed
  { spdStarted :: UTCTime,
    spdOverall :: Int,
    spdLastReport :: TVar UTCTime,
    spdLastReportPerThread :: TVar (Map ThreadId UTCTime),
    spdLastRecord :: TVar Int,
    spdLastRecordPerThread :: TVar (Map ThreadId Int)
  }

newSpeed :: MonadIO m => Int -> m Speed
newSpeed overall = do
  t <- liftIO $ getCurrentTime
  lrep <- atomically $ newTVar t
  lrept <- atomically $ newTVar M.empty
  lrec <- atomically $ newTVar 0
  lrect <- atomically $ newTVar M.empty
  pure $ Speed t overall lrep lrept lrec lrect

withProgress :: MonadIO m => Int -> Speed -> (Text -> m ()) -> m ()
withProgress curr Speed {..} f = do
  tid <- liftIO Control.Concurrent.myThreadId
  lastReportOverall <- readTVarIO spdLastReport
  mLastReportOfThread <-
    readTVarIO spdLastReportPerThread
      & fmap (M.lookup tid)
  let lastReport = fromMaybe lastReportOverall mLastReportOfThread
  let noReportTillThisMark = addUTCTime reportDelay lastReport
  t <- liftIO getCurrentTime
  when (t > noReportTillThisMark) $ do
    lastRecordOverall <- readTVarIO spdLastRecord
    lastRecordOfThread <-
      readTVarIO spdLastRecordPerThread
        & fmap (M.lookup tid)
    let lastRecord = fromMaybe lastRecordOverall lastRecordOfThread
    atomically $ do
      writeTVar spdLastReport t
      modifyTVar spdLastReportPerThread $ M.insert tid t
      writeTVar spdLastRecord curr
      modifyTVar spdLastRecordPerThread $ M.insert tid curr
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
