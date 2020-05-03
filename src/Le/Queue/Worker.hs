module Le.Queue.Worker where

import qualified Database.Persist.Postgresql as P
import qualified Le.ApiTypes.Modeled as AT
import Le.App
import Le.Import
import Le.Model

main :: Le ()
main = do
  logI <- askLogI
  runDbRemote $ do
    queueItemsRes <-
      P.selectSourceRes
        ( [ QueueStatus P.!=. AT.QueueItemStatusDone,
            QueueErrored P.!=. Just True
          ]
        )
        [P.Asc QueueCreatedAt]
    forCondRes queueItemsRes $ \queueItem -> do
      logI $ "> working with queue item " <> tshow (entityKey queueItem)
