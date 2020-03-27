module Le.CommonCrawl.Cmd where

import qualified Data.ByteString.Lazy as BL
import qualified Data.List.Split
import qualified Data.String.Class as S
import qualified Le.ApiTypes as AT
import qualified Le.CommonCrawl
import Le.Config
import Le.Import
import qualified Le.WebClient
import qualified Network.AWS.Data.Text as AWS
import qualified Network.AWS.S3 as S3
import qualified Network.URI.Encode
import Servant.Client

downloadAndFilter :: Le ()
downloadAndFilter = do
  allWarcs <- Le.CommonCrawl.listNewsWarcs
  let cheapWorkersNum = length Le.Config.cheapWorkers
  let warcChunks = Data.List.Split.chunksOf cheapWorkersNum allWarcs
  dataDir <- asks appDataDir
  forM_ (zip Le.Config.cheapWorkers warcChunks) $ \(baseUrl, warcs) -> do
    mgr <- asks appHttpManagerNoTimeout
    let cliEnv = (mkClientEnv mgr baseUrl)
    forM_ warcs $ \warc -> do
      let s3loc = "s3://" <> AWS.toText Le.CommonCrawl.newsBucket <> "/" <> (warc ^. S3.oKey . S3._ObjectKey)
      logInfo $ display $
        "> Downloading from " <> S.toText (baseUrlHost baseUrl) <> ":" <> tshow (baseUrlPort baseUrl)
          <> " file "
          <> s3loc
      bs <- Le.WebClient.cliDownloadAndFilter cliEnv (AT.DownloadAndFilterForm {dafWarcFile = s3loc})
      let keyEncoded = Network.URI.Encode.encodeText s3loc
      liftIO $ BL.writeFile (dataDir <> "/" <> S.toString keyEncoded) bs
      pure ()
