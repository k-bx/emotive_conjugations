module Le.CommonCrawl where

import Control.Lens (at)
import Control.Monad.IO.Class
--import qualified Data.ByteString as B
import qualified Data.HashMap.Monoidal as MH
import qualified Data.List
import qualified Data.String.Class as S
import qualified Data.Text as T
import Data.Warc
import Le.Import
import Network.URI
import qualified Pipes as P
import Pipes.ByteString (fromHandle)
import qualified Pipes.GZip
import Text.Pretty.Simple (pShow)

extractExampleWarc :: RIO App ()
extractExampleWarc = do
  let file = "/home/kb/tmp/emotive-conj/CC-NEWS-20180712154111-00005.warc.gz"
  domains <- newIORef (MH.fromList [])
  withRunInIO $ \runInIO -> do
    withFile file ReadMode $ \h -> do
      _ <-
        liftIO $
          iterRecords
            (\r -> runInIO (iterFunc domains r))
            (parseWarc (decompressAll (fromHandle h)))
      return ()
  domainsV <- readIORef domains
  logInfo $
    "> domains: "
      <> display
        ( pShow
            ( take
                20
                ( Data.List.sortBy
                    (flip compare `on` snd)
                    (MH.toList domainsV)
                )
            )
        )

iterFunc ::
  IORef (MH.MonoidalHashMap Text (Sum Int)) ->
  Record IO b ->
  RIO App b
iterFunc domains Record {..} = do
  let mhost =
        recHeader ^. recHeaders . at "WARC-Target-URI" <&> S.toString
          <&> parseURI
          & join
            <&> uriAuthority
          & join
            <&> uriRegName
            <&> S.toText
  case mhost of
    Nothing -> return ()
    Just root -> modifyIORef domains (MH.modify (+ 1) root)
  let mIsHttp =
        recHeader ^. recHeaders . at "Content-Type" <&> S.toText
          <&> ("application/http" `T.isInfixOf`)
  case mIsHttp of
    Nothing -> skip
    Just False -> skip
    Just True -> do
      withRunInIO $ \_runInIO -> do
        -- liftIO $ runInIO $ logInfo $ display $ tshow $ recHeader ^. recHeaders
        -- case recHeader ^. recHeaders . at "" of
        --   Just ct -> liftIO $ runInIO $ logInfo $ "Content-Type: " <> display (tshow ct)
        --   Nothing -> return ()
        r <-
          liftIO $ P.runEffect $ P.for recContent $ \_x -> do
            -- liftIO $ runInIO $ logInfo $ "Got bytes: " <> display (tshow (B.length x))
            return ()
        return r
  where
    skip = do
      r <- liftIO $ P.runEffect $ P.for recContent $ \x -> x `seq` return ()
      return r

decompressAll ::
  MonadIO m => P.Producer ByteString m r -> P.Producer ByteString m r
decompressAll p = do
  er <- Pipes.GZip.decompress' p
  case er of
    Left leftover -> decompressAll leftover
    Right r -> return r

-- |
-- > extractHostRoot "www.haskell.org"
-- "org"
extractHostRoot :: Text -> Text
extractHostRoot = Data.List.last . T.splitOn "."
