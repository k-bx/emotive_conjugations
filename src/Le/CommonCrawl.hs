module Le.CommonCrawl where

import Control.Lens ((.~), at)
import Control.Monad.IO.Class
import qualified Data.HashMap.Monoidal as MH
import qualified Data.List
import qualified Data.String.Class as S
import qualified Data.Text as T
import Data.Warc
import qualified Le.Config
import Le.Files
import Le.Import
import Le.S3Loc
import qualified Network.AWS as AWS
import qualified Network.AWS.S3 as S3
import Network.URI
import qualified Pipes as P
import qualified Pipes.ByteString
import qualified Pipes.GZip
import qualified System.Directory
import qualified System.IO

extractExampleWarc :: RIO App ()
extractExampleWarc = do
  h <- liftIO $ System.Directory.getHomeDirectory
  void $ extractWarc (h <> "/tmp/") "s3://commoncrawl/crawl-data/CC-NEWS/2018/07/CC-NEWS-20180712154111-00005.warc.gz"

extractWarc :: FilePath -> S3Loc -> Le FilePath
extractWarc tempDir loc = do
  logInfo $ "> extracting " <> display (tshow loc)
  let inPath = tempDir <> "/in.warc.gz"
  aws $ s3download loc inPath
  withFile inPath ReadMode $ \h -> do
    domains <- newIORef (MH.fromList [])
    withFile (tempDir <> "/out-uncompressed.warc") WriteMode $ \hOutUnc -> do
      withRunInIO $ \runInIO -> do
        _ <-
          liftIO $
            iterRecords
              (\r -> runInIO (iterFunc hOutUnc domains r))
              (parseWarc (decompressAll (Pipes.ByteString.fromHandle h)))
        return ()
        hSeek hOutUnc System.IO.AbsoluteSeek 0
        withFile (tempDir <> "/out.warc.gz") WriteMode $ \hOutComp -> do
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
  pure "out.warc.gz"

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

listNewsWarcsCmd :: Le ()
listNewsWarcsCmd = do
  warcs <- listNewsWarcs
  logInfo $ "> warcs: " <> display (tshow warcs)

listNewsWarcs :: Le [S3.Object]
listNewsWarcs = do
  awsEnv <- asks appAwsEnv
  runResourceT $ AWS.runAWS awsEnv $ do
    rsp <-
      AWS.send $
        S3.listObjectsV2 newsBucket
          & S3.lovPrefix .~ Just "crawl-data/CC-NEWS/2020/03/"
    pure $ rsp ^. S3.lovrsContents

newsBucket :: S3.BucketName
newsBucket = "commoncrawl"
