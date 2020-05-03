-- | BL stands for business logic
module Le.Article.BL where

import qualified Database.Persist.Postgresql as P
import Le.App
import qualified Le.Article
import Le.Import
import Le.Model
import qualified Le.Python
import qualified Le.Search
import qualified Le.Speed

repsertReceivedParseNewsPleaseRes ::
  Text ->
  Maybe Text ->
  Maybe UTCTime ->
  Int ->
  Le.Speed.Speed ->
  Le.Python.CmdParseNewsPleaseRes ->
  Le (Maybe ArticleId)
repsertReceivedParseNewsPleaseRes uriText mWarcRecId mWarcDt i speed Le.Python.CmdParseNewsPleaseRes {..} = do
  let host = Le.Article.extractHostUnsafe uriText
  case cnrLanguage of
    Just "en" -> do
      articleId <- runDb $ do
        t <- liftIO getCurrentTime
        articleId <-
          P.insert $
            Article
              { articleWarcId = mWarcRecId,
                articleWarcDate = mWarcDt,
                articleUrl = uriText,
                articleHost = host,
                articleCreatedAt = Just t
              }
        P.repsert (P.toSqlKey (P.fromSqlKey articleId)) $
          ArticlePlease
            { articlePleaseUrl = uriText,
              articlePleaseHost = host,
              articlePleaseAuthors = JsonList cnrAuthors,
              articlePleaseDateDownload =
                posixSecondsToUTCTime <$> cnrDateDownload,
              articlePleaseDatePublish =
                posixSecondsToUTCTime <$> cnrDatePublish,
              articlePleaseDateModify =
                posixSecondsToUTCTime <$> cnrDateModify,
              articlePleaseDescription = fromMaybe "" cnrDescription,
              articlePleaseFilename = cnrFilename,
              articlePleaseImageUrl = cnrImageUrl,
              articlePleaseLanguage = "en",
              articlePleaseLocalpath = cnrLocalpath,
              articlePleaseTitle = cnrTitle,
              articlePleaseTitlePage = cnrTitlePage,
              articlePleaseTitleRss = cnrTitleRss,
              articlePleaseSourceDomain = cnrSourceDomain
            }
        P.repsert (P.toSqlKey (P.fromSqlKey articleId)) $
          ArticlePleaseBig
            { articlePleaseBigMaintext = fromMaybe "" cnrMaintext,
              articlePleaseBigSpacyNer = Nothing,
              articlePleaseBigSpacyPos = Nothing
            }
        pure articleId
      Le.Speed.withProgress i speed $ \t -> do
        logInfo $ display $ "> Processing WARC: " <> t
      -- logInfo $ display $ "> URI: " <> uriText
      pure (Just articleId)
    _ -> pure Nothing

saveSpacyNer :: ArticleId -> Le.Python.CmdSpacyNerRes -> Le ()
saveSpacyNer articleId res = do
  runDb $ do
    let articlePleaseId :: ArticlePleaseId
        articlePleaseId = P.toSqlKey (P.fromSqlKey articleId)
    let articlePleaseBigId :: ArticlePleaseBigId
        articlePleaseBigId = P.toSqlKey (P.fromSqlKey articleId)
    P.update articlePleaseBigId [ArticlePleaseBigSpacyNer P.=. (Just res)]
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
