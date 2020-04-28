module Le.Queue.Handlers where

import qualified Le.ApiTypes as AT
import Le.Import
import Le.Model

queueAdd :: Entity User -> AT.QueueAddForm -> Le ()
queueAdd user AT.QueueAddForm {..} = do
  undefined
