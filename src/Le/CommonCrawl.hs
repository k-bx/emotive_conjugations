module Le.CommonCrawl where

import Control.Lens (at)
import Control.Monad.IO.Class
import qualified Data.ByteString as B
import Data.Warc
import Le.Import
import qualified Pipes as P
import Pipes.ByteString (fromHandle)

extractExampleWarc :: RIO App ()
extractExampleWarc = do
  let file = "/home/kb/tmp/emotive-conj/CC-NEWS-20180712154111-00005.warc.gz"
  withRunInIO $ \runInIO -> do
    withFile file ReadMode $ \h -> do
      _ <- liftIO $ iterRecords (\r -> runInIO (iterFunc r)) (parseWarc (fromHandle h))
      return ()

iterFunc :: Record IO b -> RIO App b
iterFunc Record {..} = do
  withRunInIO $ \runInIO -> do
    case recHeader ^. recHeaders . at "Content-Type" of
      Just ct -> liftIO $ runInIO $ logInfo $ "Content-Type: " <> display (tshow ct)
      Nothing -> return ()
    r <-
      liftIO $ P.runEffect $ P.for recContent $ \x -> do
        liftIO $ runInIO $ logInfo $ "Got bytes: " <> display (tshow (B.length x))
        return ()
    return r
