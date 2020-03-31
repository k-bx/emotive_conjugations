module Le.CommonCrawl.Cmd where

import Control.Lens (at)
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.Split
import qualified Data.String.Class as S
import Data.Warc
import qualified Le.ApiTypes as AT
import Le.CommonCrawl
import qualified Le.Config
import Le.Import
import qualified Le.Python
import qualified Le.WebClient
import qualified Network.AWS.Data.Text as AWS
import qualified Network.AWS.S3 as S3
import qualified Network.URI.Encode
import Servant.Client
import qualified System.Directory

downloadAndFilter :: Le ()
downloadAndFilter = do
  allWarcs0 <- Le.CommonCrawl.listNewsWarcs
  cfg <- asks appConfig
  let filteredDataDir = Le.Config.filteredDataDir cfg
  let s3loc warc = "s3://" <> AWS.toText Le.CommonCrawl.newsBucket <> "/" <> (warc ^. S3.oKey . S3._ObjectKey)
  let outPath warc =
        filteredDataDir <> "/" <> S.toString (Network.URI.Encode.encodeText (s3loc warc))
  allWarcs <- fmap catMaybes $ forM allWarcs0 $ \warc -> do
    liftIO $
      System.Directory.doesFileExist (outPath warc) >>= \case
        False -> pure $ Just warc
        True -> pure Nothing
  let cheapWorkersNum = length Le.Config.cheapWorkers
  logInfo $ display $
    "> About to process " <> tshow (length allWarcs) <> " warcs"
  let chunkSize = (length allWarcs `div` cheapWorkersNum) + 1
  let warcChunks = Data.List.Split.chunksOf chunkSize allWarcs
  logInfo $ display $ "> Chunk size: " <> tshow chunkSize
  pooledForConcurrently_ (zip Le.Config.cheapWorkers warcChunks) $ \(baseUrl, warcs) -> do
    mgr <- asks appHttpManagerNoTimeout
    let cliEnv = (mkClientEnv mgr baseUrl)
    forM_ warcs $ \warc -> do
      logInfo $ display $
        "> Downloading from " <> S.toText (baseUrlHost baseUrl) <> ":" <> tshow (baseUrlPort baseUrl)
          <> " file "
          <> s3loc warc
      bs <- Le.WebClient.cliDownloadAndFilter cliEnv (AT.DownloadAndFilterForm {dafWarcFile = s3loc warc})
      liftIO $ BL.writeFile (outPath warc) bs
      pure ()

testDownloadAndFilter :: Le ()
testDownloadAndFilter = do
  dataDir <- asks appDataDir
  let baseUrl = BaseUrl
        { baseUrlScheme = Http,
          baseUrlHost = "localhost",
          baseUrlPort = 6666,
          baseUrlPath = ""
        }
  -- let baseUrl = Data.List.head Le.Config.cheapWorkers
  mgr <- asks appHttpManagerNoTimeout
  let cliEnv = (mkClientEnv mgr baseUrl)
  bs <- Le.WebClient.cliTestDownloadAndFilter cliEnv
  logInfo $ display $ "> testDownloadAndFilter got bs: " <> tshow (BL.length bs)
  let keyEncoded = Network.URI.Encode.encodeText "testresult.warc.gz"
  liftIO $ BL.writeFile (dataDir <> "/" <> S.toString keyEncoded) bs
  pure ()

parseFilteredArticles :: Le ()
parseFilteredArticles = do
  cfg <- asks appConfig
  filteredWarcPaths <- liftIO $ System.Directory.listDirectory (Le.Config.filteredDataDir cfg)
  forM_ filteredWarcPaths $ \warcPath -> do
    recs <- allWarcRecords (Le.Config.filteredDataDir cfg <> "/" <> warcPath)
    logInfo $ display $ "> warc path: " <> S.toText warcPath
    forM_ recs $ \(recHeader, recBs) -> do
      let uriText =
            recHeader
              ^. recHeaders
              . at "WARC-Target-URI"
              & fromMaybe ""
              & S.toText
      res <- Le.Python.runPythonParsing (Le.Python.CmdParseArticle (Le.Python.CmdParseArticleOpts (S.toText recBs)))
      logInfo $ display $ "> URI: " <> uriText
      logInfo $ display $ "> Title: " <> tshow (Le.Python.cprTitle res)
      logInfo $ display $ "> Pub date: " <> tshow (fmap posixSecondsToUTCTime (Le.Python.cprPubDate res))
-- logInfo $ display $ "> text: " <> Le.Python.cprText res
