module Le.CommonCrawl where

import Control.Lens ((.~), at)
import Control.Monad.IO.Class
import qualified Data.HashMap.Monoidal as MH
import qualified Data.List
import qualified Data.String.Class as S
import qualified Data.Text as T
import Data.Warc
import qualified Le.Config
import Le.Import
import qualified Network.AWS as AWS
import qualified Network.AWS.S3 as S3
import Network.URI
import qualified Pipes as P
import qualified Pipes.ByteString
import qualified Pipes.GZip
import qualified System.IO

-- import Text.Pretty.Simple (pShow)

extractExampleWarc :: RIO App ()
extractExampleWarc = do
  let file = "/home/kb/tmp/emotive-conj/CC-NEWS-20180712154111-00005.warc.gz"
      -- fileOutUncompressed = "/home/kb/tmp/emotive-conj/CC-NEWS-20180712154111-00005-filtered.warc"
      fileOutCompressed = "/home/kb/tmp/emotive-conj/CC-NEWS-20180712154111-00005-filtered.warc.gz"
  domains <- newIORef (MH.fromList [])
  withSystemTempFile "conj-warc-tmp" $ \_fpath hOutUnc -> do
    withRunInIO $ \runInIO -> do
      withFile file ReadMode $ \h -> do
        _ <-
          liftIO $
            iterRecords
              (\r -> runInIO (iterFunc hOutUnc domains r))
              (parseWarc (decompressAll (Pipes.ByteString.fromHandle h)))
        return ()
      hSeek hOutUnc System.IO.AbsoluteSeek 0
      withFile fileOutCompressed WriteMode $ \hOutComp -> do
        liftIO $ P.runEffect $
          ( Pipes.GZip.compress
              Pipes.GZip.defaultCompression
              (Pipes.ByteString.fromHandle hOutUnc)
          )
            P.>-> (Pipes.ByteString.toHandle hOutComp)
    domainsV <- readIORef domains
    logInfo $
      "> domains: "
        <> display
          ( tshow
              ( take
                  100
                  ( Data.List.sortBy
                      (flip compare `on` snd)
                      (MH.toList domainsV)
                  )
              )
          )

iterFunc ::
  Handle ->
  IORef (MH.MonoidalHashMap Text (Sum Int)) ->
  Record IO b ->
  RIO App b
iterFunc hOut domains record@Record {..} = do
  let mhost =
        recHeader ^. recHeaders . at "WARC-Target-URI" <&> S.toString
          <&> parseURI
          & join
            <&> uriAuthority
          & join
            <&> uriRegName
            <&> S.toText
  let mIsHttp =
        recHeader ^. recHeaders . at "Content-Type" <&> S.toText
          <&> ("application/http" `T.isInfixOf`)
  let newsHostsWWW = map ("www." <>) Le.Config.newsHosts
  case mIsHttp of
    Nothing -> skip
    Just False -> skip
    Just True -> do
      case mhost of
        Nothing -> skip
        Just host -> do
          modifyIORef domains (MH.modify (+ 1) host)
          if host `elem` Le.Config.newsHosts
            || host `elem` newsHostsWWW
            then do
              liftIO $ P.runEffect $ encodeRecord record P.>-> Pipes.ByteString.toHandle hOut
            else skip
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

listNewsWarcs :: RIO App ()
listNewsWarcs = do
  awsEnv <- asks appAwsEnv
  withRunInIO $ \runInIO -> do
    runResourceT $ AWS.runAWS awsEnv $ do
      rsp <-
        AWS.send $
          S3.listObjectsV2 "commoncrawl"
            & S3.lovPrefix .~ Just "crawl-data/CC-NEWS/2020/03/"
      liftIO $ runInIO $ logInfo $ display $ tshow $
        (map (^. S3.oKey) (rsp ^. S3.lovrsContents :: [S3.Object]))
