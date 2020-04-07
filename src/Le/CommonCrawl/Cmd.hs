module Le.CommonCrawl.Cmd where

import Control.Lens (at)
import qualified Data.ByteString.Lazy as BL
import qualified Data.List
import qualified Data.String.Class as S
import qualified Data.Text as T
import Data.Warc
import qualified Database.Persist.Postgresql as P
import qualified Le.ApiTypes as AT
import Le.App
import qualified Le.Article
import Le.CommonCrawl
import qualified Le.Config
import Le.Import
import Le.Model
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
  pooledForConcurrentlyN_ cheapWorkersNum (zip (Data.List.cycle Le.Config.cheapWorkers) allWarcs) $ \(baseUrl, warc) -> do
    mgr <- asks appHttpManagerNoTimeout
    let cliEnv = mkClientEnv mgr baseUrl
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
  app <- ask
  filteredWarcPaths <- liftIO $ System.Directory.listDirectory (Le.Config.filteredDataDir cfg)
  pooledForConcurrentlyN_ (appNumCapabilities app) filteredWarcPaths $ \warcPath -> do
    recs <- allWarcRecords (Le.Config.filteredDataDir cfg <> "/" <> warcPath)
    logInfo $ display $ "> warc path: " <> S.toText warcPath
    forM_ recs $ \(recHeader, recBs) -> do
      let html = T.strip (S.toText recBs)
      if html == ""
        then pure ()
        else do
          let uriText =
                recHeader
                  ^. recHeaders
                  . at "WARC-Target-URI"
                  & fromMaybe ""
                  & S.toText
          res <- Le.Python.cmdParseArticle (Le.Python.CmdParseArticleOpts html)
          void $ runDb $ do
            -- articleId <- ensureArticle uriText
            P.insert $ ArticleNp
              { articleNpUrl = uriText,
                articleNpHost = Le.Article.extractHostUnsafe uriText,
                articleNpTitle = Le.Python.cprTitle res,
                articleNpAuthors = JsonList (Le.Python.cprAuthors res),
                articleNpDate =
                  posixSecondsToUTCTime <$> Le.Python.cprPubDate res,
                articleNpContent = Le.Python.cprText res,
                articleNpLang = Le.Python.cprLanguage res,
                articleNpSpacyNer = Nothing
              }
          logInfo $ display $ "> URI: " <> uriText
          -- logInfo $ display $ "> Title: " <> tshow (Le.Python.cprTitle res)
          -- logInfo $ display $ "> Pub date: " <> tshow (fmap posixSecondsToUTCTime (Le.Python.cprPubDate res))
          -- logInfo $ display $ "> text: " <> Le.Python.cprText res
          pure ()

-- ensureArticle :: Text -> ReaderT P.SqlBackend IO ArticleId
-- ensureArticle uriText = do
--   mArticle <- P.getBy (ArticleUrlUniq uriText)
--   case mArticle of
--     Nothing -> P.insert $ Article
--       { articleUrl = uriText,
--         articleHost = Le.Article.extractHostUnsafe uriText
--       }
--     Just article -> pure $ entityKey article

spacyNerArticles :: Le ()
spacyNerArticles = do
  articleNps <- runDb $ P.selectList [ArticleNpSpacyNer P.==. Nothing] [P.Desc ArticleNpId]
  pooledForConcurrentlyN_ Le.Config.numPythonWorkers articleNps $ \articleNp -> do
    res <- Le.Python.cmdSpacyNer (Le.Python.CmdSpacyNerOpts (articleNpContent (ev articleNp)))
    runDb $ do
      P.update (entityKey articleNp) [ArticleNpSpacyNer P.=. (Just res)]
      forM_ (Le.Python.csrEnts res) $ \Le.Python.CmdSpacyNerResEnt {..} -> do
        P.insert $ NamedEntity
          { namedEntityArticleId = P.toSqlKey (P.fromSqlKey (entityKey articleNp)),
            namedEntityEntity = cseText,
            namedEntityStart = cseStart,
            namedEntityStartChar = cseStartChar,
            namedEntityEnd = cseEnd,
            namedEntityEndChar = cseEndChar,
            namedEntityLabel_ = cseLabel_
          }
