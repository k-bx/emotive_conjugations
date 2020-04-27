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
import Le.AppUtils
import qualified Le.Article
import Le.CommonCrawl
import qualified Le.Config
import qualified Le.Html
import Le.Import
import Le.Model
import qualified Le.Python
import qualified Le.Search
import qualified Le.Speed
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
  speed <- Le.Speed.newSpeed (length allWarcs)
  let cheapWorkersNum = length Le.Config.cheapWorkers
  logInfo $ display $
    "> About to process " <> tshow (length allWarcs) <> " warcs"
  pooledForConcurrentlyN_ cheapWorkersNum (zip [1 ..] (zip (Data.List.cycle Le.Config.cheapWorkers) allWarcs)) $ \(i, (baseUrl, warc)) ->
    logOnError $ do
      logInfo $ display $
        "> Downloading from " <> S.toText (baseUrlHost baseUrl) <> ":" <> tshow (baseUrlPort baseUrl)
          <> " file "
          <> s3loc warc
      Le.Speed.withProgress i speed $ \t -> do
        logInfo $ display $ "> Progress: " <> t
      mgr <- asks appHttpManagerNoTimeout
      let cliEnv = mkClientEnv mgr baseUrl
      bs <- Le.WebClient.cliDownloadAndFilter cliEnv (AT.DownloadAndFilterForm {dafWarcFile = s3loc warc})
      logInfo $ display $ "> writing " <> (S.toText (outPath warc))
      liftIO $ BL.writeFile (outPath warc) bs
      pure ()

testDownloadAndFilter :: Le ()
testDownloadAndFilter = do
  dataDir <- asks appDataDir
  let baseUrl =
        BaseUrl
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
  speed <- Le.Speed.newSpeed (length filteredWarcPaths)
  pooledForConcurrentlyN_ (appNumCapabilities app) (zip [1 ..] filteredWarcPaths) (processWarc cfg speed)
  where
    processWarc cfg speed (i, warcPath) = do
      Le.Speed.withProgress i speed $ \t -> do
        logInfo $ display $ "> Processing WARC: " <> t
      recs <- allWarcRecords (Le.Config.filteredDataDir cfg <> "/" <> warcPath)
      logInfo $ display $ "> warc path: " <> S.toText warcPath
      forM_ recs $ \(recHeader, recBs) -> do
        -- logInfo $ "> headersAndhtml: " <> display (T.take 2000 headersAndhtml)
        let (_headers, html0) = Le.Html.splitHeadersAndHtml recBs
        let html = T.strip html0
        -- logInfo $ "> html: " <> display (T.take 50 html)
        -- logInfo $ "> html length: " <> display (T.length html)
        -- logInfo $ "> recBs: " <> display (T.take 2550 (S.toText recBs))
        if T.length html == 0
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
              mArticlePresent <- runDb $ P.selectFirst [ArticleWarcId P.==. Just warcRecId] []
              case mArticlePresent of
                Just _ -> pure ()
                Nothing -> runCmd speed i warcRecId host html uriText warcDt
    runCmd speed i warcRecId host html uriText warcDt = do
      res <- Le.Python.cmdParseNewsPlease (Le.Python.CmdParseNewsPleaseOpts html uriText (utcTimeToPOSIXSeconds warcDt))
      case Le.Python.cnrLanguage res of
        Just "en" -> do
          void $ runDb $ do
            articleId <-
              P.insert $
                Article
                  { articleWarcId = Just warcRecId,
                    articleWarcDate = warcDt,
                    articleUrl = uriText,
                    articleHost = host
                  }
            P.repsert (P.toSqlKey (P.fromSqlKey articleId)) $
              ArticlePlease
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
                  articlePleaseLanguage = "en",
                  articlePleaseLocalpath = Le.Python.cnrLocalpath res,
                  articlePleaseTitle = Le.Python.cnrTitle res,
                  articlePleaseTitlePage = Le.Python.cnrTitlePage res,
                  articlePleaseTitleRss = Le.Python.cnrTitleRss res,
                  articlePleaseSourceDomain = Le.Python.cnrSourceDomain res
                }
            P.repsert (P.toSqlKey (P.fromSqlKey articleId)) $
              ArticlePleaseBig
                { articlePleaseBigMaintext = fromMaybe "" (Le.Python.cnrMaintext res),
                  articlePleaseBigSpacyNer = Nothing,
                  articlePleaseBigSpacyPos = Nothing
                }
          Le.Speed.withProgress i speed $ \t -> do
            logInfo $ display $ "> Processing WARC: " <> t
          -- logInfo $ display $ "> URI: " <> uriText
          pure ()
        _ -> pure ()

spacyNerArticles :: Le ()
spacyNerArticles = do
  articlePleaseBigs <- runDb $ P.selectList [ArticlePleaseBigSpacyNer P.==. Nothing] [P.Desc ArticlePleaseBigId]
  speed <- Le.Speed.newSpeed (length articlePleaseBigs)
  pooledForConcurrentlyN_ Le.Config.numPythonWorkers (zip [1 ..] articlePleaseBigs) $ \(i, articlePleaseBig) -> do
    Le.Speed.withProgress i speed $ \t -> do
      logInfo $ display $ "> Processing ner article: " <> t
    res <- Le.Python.cmdSpacyNer (Le.Python.CmdSpacyNerOpts (articlePleaseBigMaintext (ev articlePleaseBig)))
    runDb $ do
      let articlePleaseId :: ArticlePleaseId
          articlePleaseId = P.toSqlKey (P.fromSqlKey (entityKey articlePleaseBig))
      P.update (entityKey articlePleaseBig) [ArticlePleaseBigSpacyNer P.=. (Just res)]
      forM_ (Le.Python.csrEnts res) $ \Le.Python.CmdSpacyNerResEnt {..} -> do
        when (cseLabel_ == "PERSON") $ do
          let (search1, search2, search3) = Le.Search.computeSearchTerms cseText
          void $ P.insert $
            NamedEntity
              { namedEntityArticlePleaseId = P.toSqlKey (P.fromSqlKey articlePleaseId),
                namedEntityEntity = cseText,
                namedEntityStart = cseStart,
                namedEntityStartChar = cseStartChar,
                namedEntityEnd = cseEnd,
                namedEntityEndChar = cseEndChar,
                namedEntityLabel_ = cseLabel_,
                namedEntitySearch1 = Just search1,
                namedEntitySearch2 = Just search2,
                namedEntitySearch3 = Just search3,
                namedEntityCanonical = Just (Le.Search.namedEntityCanonicalForm cseText),
                namedEntityProper = Nothing
              }
  Le.Search.reindexProper

spacyPosArticles :: Le ()
spacyPosArticles = do
  articleBigNps <- runDb $ P.selectList [ArticlePleaseBigSpacyPos P.==. Nothing] [P.Desc ArticlePleaseBigId]
  speed <- Le.Speed.newSpeed (length articleBigNps)
  pooledForConcurrentlyN_ Le.Config.numPythonWorkers (zip [1 ..] articleBigNps) $ \(i, articleBigNp) -> do
    Le.Speed.withProgress i speed $ \t -> do
      logInfo $ display $ "> Processing pos article: " <> t
    res <- Le.Python.cmdSpacyPos (Le.Python.CmdSpacyPosOpts (articlePleaseBigMaintext (ev articleBigNp)))
    runDb $ do
      P.update (entityKey articleBigNp) [ArticlePleaseBigSpacyPos P.=. (Just res)]
