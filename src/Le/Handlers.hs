module Le.Handlers where

import qualified Data.ByteString.Lazy as BL
import qualified Le.ApiTypes as AT
import Le.AppUtils
import qualified Le.CommonCrawl
import Le.Import
import qualified Le.S3Loc
import Le.Util
import qualified System.Directory

ping :: HasCallStack => RIO App Text
ping = sg $ return "pong"

pingJson :: HasCallStack => RIO App [Text]
pingJson = sg $ do pure $ ["pong"]

errorOut :: HasCallStack => RIO App [Text]
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
