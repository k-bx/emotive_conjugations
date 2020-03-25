module Le.Handlers where

import qualified Data.ByteString.Lazy as BL
import qualified Le.ApiTypes as AT
import Le.AppUtils
import qualified Le.CommonCrawl
import Le.Import
import qualified Le.S3Loc
import Le.Util

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
  tempDir <- asks appTempDir
  outPath <- Le.CommonCrawl.extractWarc tempDir (eitherErr (Le.S3Loc.parseS3Url dafWarcFile))
  liftIO $ BL.readFile outPath
