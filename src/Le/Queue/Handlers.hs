module Le.Queue.Handlers where

import qualified Database.Persist.Postgresql as P
import qualified Le.ApiTypes as AT
import qualified Le.ApiTypes.Modeled as AT
import Le.App
import Le.AppUtils
import Le.Import
import Le.Model
import Le.Time

queueAdd :: Entity User -> AT.QueueAddForm -> Le ()
queueAdd user AT.QueueAddForm {..} = sg $ do
  t <- liftIO $ getCurrentTime
  _ <-
    runDb $ P.insert $
      Queue
        { queueUserId = entityKey user,
          queueUrl = qafUrl,
          queueStatus = AT.QueueItemStatusQueued,
          queueCreatedAt = t,
          queueUpdatedAt = t
        }
  pure ()

queueList :: Entity User -> Le [AT.QueueItem]
queueList _user = sg $ do
  queueItems <-
    runDb $
      P.selectList
        []
        [P.Desc QueueCreatedAt, P.LimitTo 100]
  forM queueItems $ \queueItem -> do
    pure $
      AT.QueueItem
        { quiId = entityKey queueItem,
          quiUserId = queueUserId (ev queueItem),
          quiStatus = queueStatus (ev queueItem),
          quiCreatedAt = renderTime (queueCreatedAt (ev queueItem)),
          quiUpdatedAt = renderTime (queueUpdatedAt (ev queueItem))
        }
