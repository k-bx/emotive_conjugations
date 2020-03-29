module Le.CommonCrawl where

import Control.Lens ((.~), at)
import Control.Monad.IO.Class
import qualified Data.HashMap.Monoidal as MH
import qualified Data.List
import qualified Data.String.Class as S
import qualified Data.Text as T
import Data.Warc
import Le.App
import qualified Le.Config
import Le.Files
import Le.Import
import Le.S3Loc
import qualified Network.AWS as AWS
import qualified Network.AWS.S3 as S3
import Network.URI
import qualified Network.URI.Encode
import qualified Pipes as P
import qualified Pipes.ByteString
import qualified Pipes.GZip
import qualified System.Directory

extractExampleWarc :: RIO App ()
extractExampleWarc = do
  h <- liftIO $ System.Directory.getHomeDirectory
  void $ extractWarc (h <> "/tmp/") "s3://commoncrawl/crawl-data/CC-NEWS/2018/07/CC-NEWS-20180712154111-00005.warc.gz"

extractWarc :: FilePath -> S3Loc -> Le FilePath
extractWarc tempDir loc = do
  logInfo $ "> extracting " <> display (tshow loc)
  let locEncoded = Network.URI.Encode.encodeText (Le.S3Loc.toCliS3Url loc)
  let inPath = tempDir <> "/" <> S.toString locEncoded <> "-in.warc.gz"
      outPathUnc = tempDir <> "/" <> S.toString locEncoded <> "-out-uncompressed.warc"
      outPath = tempDir <> "/" <> S.toString locEncoded <> "-out.warc.gz"
  aws $ s3download loc inPath
  withFile inPath ReadMode $ \h -> do
    domains <- newIORef (MH.fromList [])
    withFile outPathUnc WriteMode $ \hOutUnc -> do
      withRunInIO $ \runInIO -> do
        _ <-
          liftIO $
            iterRecords
              (\r -> runInIO (iterFunc hOutUnc domains r))
              (parseWarc (decompressAll (Pipes.ByteString.fromHandle h)))
        return ()
    withFile outPathUnc ReadMode $ \hOutUnc -> do
      withFile outPath WriteMode $ \hOutComp -> do
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
                ( map
                    (\(a, b) -> (a, getSum b))
                    ( take
                        50
                        ( Data.List.sortBy
                            (flip compare `on` snd)
                            (MH.toList domainsV)
                        )
                    )
                )
            )
  liftIO $ System.Directory.removeFile inPath
  liftIO $ System.Directory.removeFile outPathUnc
  logInfo $ "> done extractng. result at: " <> display (tshow outPath)
  pure $ outPath

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
  case mIsHttp of
    Nothing -> skip
    Just False -> skip
    Just True -> do
      case mhost of
        Nothing -> skip
        Just host -> do
          modifyIORef domains (MH.modify (+ 1) host)
          if any (`T.isInfixOf` host) Le.Config.newsHosts
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
