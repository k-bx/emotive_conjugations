module Le.CommonCrawl.Cmd where

import Control.Lens (at)
import qualified Data.ByteString.Lazy as BL
import qualified Data.List
import qualified Data.String.Class as S
import qualified Data.Text as T
import qualified Data.Time.Format
import Data.Warc
import qualified Database.Persist.Postgresql as P
import qualified Le.ApiTypes as AT
import Le.App
import qualified Le.Article
import Le.CommonCrawl
import qualified Le.Config
import qualified Le.Html
import Le.Import
import Le.Model
import qualified Le.Python
import qualified Le.WebClient
import qualified Network.AWS.Data.Text as AWS
import qualified Network.AWS.S3 as S3
import qualified Network.URI.Encode
import qualified Safe
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
      let headersAndhtml = T.strip (S.toText recBs)
      -- logInfo $ "> headersAndhtml: " <> display (T.take 2000 headersAndhtml)
      let (_headers, html0) = Le.Html.splitHeadersAndHtml headersAndhtml
      let html = T.strip html0
      -- logInfo $ "> html: " <> display (T.take 2000 html)
      if html == ""
        then pure ()
        else do
          let uriText =
                recHeader ^. recHeaders
                  . at "WARC-Target-URI"
                  & fromMaybe ""
                  & S.toText
          let host = Le.Article.extractHostUnsafe uriText
          when (any (`T.isInfixOf` host) Le.Config.newsHosts) $ do
            let warcRecId =
                  recHeader ^. recHeaders
                    . at "WARC-Record-ID"
                    & Safe.fromJustNote "Warc record ID is empty"
                    & S.toText
            let warcDt =
                  recHeader ^. recHeaders
                    . at "WARC-Date"
                    & fromMaybe ""
                    & S.toString
                    & Data.Time.Format.parseTimeM False Data.Time.Format.defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"
                    & Safe.fromJustNote "Couldn't parse time"
            res <- Le.Python.cmdParseNewsPlease (Le.Python.CmdParseNewsPleaseOpts html uriText (utcTimeToPOSIXSeconds warcDt))
            case Le.Python.cnrLanguage res of
              "en" -> do
                void $ runDb $ do
                  articleId <- P.insert $ Article
                    { articleWarcId = warcRecId,
                      articleWarcDate = warcDt,
                      articleUrl = uriText,
                      articleHost = host
                    }
                  P.repsert (P.toSqlKey (P.fromSqlKey articleId)) $ ArticlePlease
                    { articlePleaseUrl = uriText,
                      articlePleaseHost = host,
                      articlePleaseAuthors = JsonList (Le.Python.cnrAuthors res),
                      articlePleaseDateDownload =
                        posixSecondsToUTCTime <$> Le.Python.cnrDateDownload res,
                      articlePleaseDatePublish =
                        posixSecondsToUTCTime <$> Le.Python.cnrDatePublish res,
                      articlePleaseDateModify =
                        posixSecondsToUTCTime <$> Le.Python.cnrDateModify res,
                      articlePleaseDescription = fromMaybe "" (Le.Python.cnrDescription res),
                      articlePleaseFilename = Le.Python.cnrFilename res,
                      articlePleaseImageUrl = Le.Python.cnrImageUrl res,
                      articlePleaseLanguage = Le.Python.cnrLanguage res,
                      articlePleaseLocalpath = Le.Python.cnrLocalpath res,
                      articlePleaseTitle = Le.Python.cnrTitle res,
                      articlePleaseTitlePage = Le.Python.cnrTitlePage res,
                      articlePleaseTitleRss = Le.Python.cnrTitleRss res,
                      articlePleaseSourceDomain = Le.Python.cnrSourceDomain res,
                      articlePleaseMaintext = fromMaybe "" (Le.Python.cnrMaintext res),
                      articlePleaseSpacyNer = Nothing,
                      articlePleaseSpacyPos = Nothing
                    }
                logInfo $ display $ "> URI: " <> uriText
                -- logInfo $ display $ "> Title: " <> tshow (Le.Python.cprTitle res)
                -- logInfo $ display $ "> Pub date: " <> tshow (fmap posixSecondsToUTCTime (Le.Python.cprPubDate res))
                -- logInfo $ display $ "> text: " <> Le.Python.cprText res
                pure ()
              _ -> pure ()

spacyNerArticles :: Le ()
spacyNerArticles = do
  articlePleases <- runDb $ P.selectList [ArticlePleaseSpacyNer P.==. Nothing] [P.Desc ArticlePleaseId]
  pooledForConcurrentlyN_ Le.Config.numPythonWorkers articlePleases $ \articlePlease -> do
    res <- Le.Python.cmdSpacyNer (Le.Python.CmdSpacyNerOpts (articlePleaseMaintext (ev articlePlease)))
    runDb $ do
      P.update (entityKey articlePlease) [ArticlePleaseSpacyNer P.=. (Just res)]
      forM_ (Le.Python.csrEnts res) $ \Le.Python.CmdSpacyNerResEnt {..} -> do
        P.insert $ NamedEntity
          { namedEntityArticlePleaseId = P.toSqlKey (P.fromSqlKey (entityKey articlePlease)),
            namedEntityEntity = cseText,
            namedEntityStart = cseStart,
            namedEntityStartChar = cseStartChar,
            namedEntityEnd = cseEnd,
            namedEntityEndChar = cseEndChar,
            namedEntityLabel_ = cseLabel_
          }

spacyPosArticles :: Le ()
spacyPosArticles = do
  articleNps <- runDb $ P.selectList [ArticlePleaseSpacyPos P.==. Nothing] [P.Desc ArticlePleaseId]
  -- articleNps <- runDb $ P.selectList [ArticleNpSpacyPos P.==. Nothing] [P.Desc ArticleNpId, P.LimitTo 10]
  pooledForConcurrentlyN_ Le.Config.numPythonWorkers articleNps $ \articleNp -> do
    res <- Le.Python.cmdSpacyPos (Le.Python.CmdSpacyPosOpts (articlePleaseMaintext (ev articleNp)))
    runDb $ do
      P.update (entityKey articleNp) [ArticlePleaseSpacyPos P.=. (Just res)]
