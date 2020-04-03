module Le.Handlers where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.IO as T
import qualified Le.ApiTypes as AT
import Le.AppUtils
import qualified Le.CommonCrawl
import Le.Import
import Le.Model
import qualified Le.S3Loc
import Le.Util
import qualified Servant.Types.SourceT
import qualified System.Directory
import qualified Prelude

logErrorHandler :: HasCallStack => Maybe Text -> AppM ()
logErrorHandler mMsg =
  sg $ do
    logError $ display $ "Error reported via API: " <> tshow mMsg

index :: HasCallStack => Entity User -> AppM Text
index _user = sg $ liftIO $ T.readFile "index.html"

indexNoAuth :: HasCallStack => AppM Text
indexNoAuth = sg $ liftIO $ T.readFile "index.html"

ping :: HasCallStack => Le Text
ping = sg $ return "pong"

pingJson :: HasCallStack => Le [Text]
pingJson = sg $ do pure $ ["pong"]

errorOut :: HasCallStack => Le [Text]
errorOut = sg $ do pure $ error "Something bad happened"

downloadAndFilter ::
  HasCallStack =>
  AT.DownloadAndFilterForm ->
  RIO App BL.ByteString
downloadAndFilter AT.DownloadAndFilterForm {..} = sg $ do
  logInfo $ "> downloadAndFilter"
  tempDir <- asks appTempDir
  outPath <- Le.CommonCrawl.extractWarc tempDir (eitherErr (Le.S3Loc.parseS3Url dafWarcFile))
  liftIO $ BL.readFile outPath

testDownloadAndFilter ::
  HasCallStack =>
  RIO App BL.ByteString
testDownloadAndFilter = sg $ do
  logInfo $ "> testDownloadAndFilter"
  h <- liftIO $ System.Directory.getHomeDirectory
  let outPath = h <> "/tmp/s3%3A%2F%2Fcommoncrawl%2Fcrawl-data%2FCC-NEWS%2F2020%2F03%2FCC-NEWS-20200301015020-01025.warc.gz-out.warc.gz"
  liftIO $ BL.readFile outPath

testDownloadAndFilter2 ::
  HasCallStack =>
  RIO App (Servant.Types.SourceT.SourceT IO ByteString)
testDownloadAndFilter2 = do
  logInfo $ "> testDownloadAndFilter"
  liftIO $ Prelude.putStrLn $ "> testDownloadAndFilter2"
  h <- liftIO $ System.Directory.getHomeDirectory
  let outPath = h <> "/tmp/s3%3A%2F%2Fcommoncrawl%2Fcrawl-data%2FCC-NEWS%2F2020%2F03%2FCC-NEWS-20200301015020-01025.warc.gz-out.warc.gz"
  pure $ Servant.Types.SourceT.readFile outPath
