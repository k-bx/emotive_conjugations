{-# LANGUAGE QuasiQuotes #-}

module Le.Queue.Worker where

import qualified Data.List
import qualified Data.Text as T
import qualified Database.Esqueleto as E
import qualified Database.Persist.Postgresql as P
import qualified Le.ApiTypes.Modeled as AT
import Le.App
import Le.AppUtils
import qualified Le.Article.BL
import Le.Import
import Le.Model
import qualified Le.Python
import qualified Le.Speed

main :: Le ()
main = forever $ do
  go
  liftIO $ threadDelaySecs 5
  where
    go = sg $ do
      [P.Single queueItemsLen] <-
        runDb $
          E.rawSql
            [q|select count(*) from queue where status != 'done' and errored != true|]
            []
      speed <- Le.Speed.newSpeed queueItemsLen
      runDb $ do
        queueItemsRes <-
          P.selectSourceRes
            ( [ QueueStatus P.!=. AT.QueueItemStatusDone,
                QueueErrored P.!=. Just True
              ]
            )
            [P.Asc QueueCreatedAt]
        forCondResEnum queueItemsRes $ \(i, queueItem) -> flip catchAny (onErr queueItem) $ do
          processItem queueItem i speed
      where
        onErr :: Entity Queue -> SomeException -> ReaderT P.SqlBackend Le ()
        onErr queueItem e = do
          lift $
            logError $
              display $
                "> error while downloading queueItem: "
                  <> tshow @Int64 (P.fromSqlKey (entityKey queueItem))
                  <> "; error: "
                  <> tshow e
          P.update (entityKey queueItem) [QueueErrored P.=. Just True]

processItem ::
  Entity Queue ->
  Int ->
  Le.Speed.Speed ->
  ReaderT P.SqlBackend Le ()
processItem queueItem i speed = do
  logI <- lift askLogI
  let queueItemId = entityKey queueItem
  logI $ "> working with queue item: " <> tshow @Int64 (P.fromSqlKey queueItemId) <> "; status: " <> tshow (queueStatus (ev queueItem))
  logI $ "> downloading..." <> tshow @Int64 (P.fromSqlKey queueItemId) <> "; status: " <> tshow (queueStatus (ev queueItem))
  P.update queueItemId [QueueStatus P.=. AT.QueueItemStatusDownloading]
  resDownload <-
    lift $
      Le.Python.cmdDownloadUrlNewsPlease $
        Le.Python.CmdDownloadUrlNewsPleaseOpts {cduUrl = queueUrl (ev queueItem)}
  mArticleId <- lift $ Le.Article.BL.repsertReceivedParseNewsPleaseRes (queueUrl (ev queueItem)) Nothing Nothing i speed resDownload
  articleId <- mustFindE mArticleId
  processDownloadedItem
    (Just queueItem)
    articleId
    (Le.Python.cnrTitle resDownload)
    (Le.Python.cnrMaintext resDownload)

processDownloadedItem ::
  Maybe (Entity Queue) ->
  ArticleId ->
  Maybe Text ->
  Maybe Text ->
  ReaderT P.SqlBackend Le ()
processDownloadedItem mQueueItem articleId mTitle mMainText = do
  -- lift $ logInfo $ display $ "> processDownloadedItem: " <> tshow articleId
  whenQueueItem $ \qi ->
    P.update (entityKey qi) [QueueStatus P.=. AT.QueueItemStatusExtracting]
  whenQueueItem $ \qi ->
    P.update (entityKey qi) [QueueStatus P.=. AT.QueueItemStatusNer]
  -- articleId <- mustFindE mArticleId
  whenQueueItem $ \qi ->
    P.update (entityKey qi) [QueueArticleId P.=. Just articleId]
  case mMainText of
    Nothing -> do
      whenQueueItem $ \qi ->
        P.update (entityKey qi) [QueueStatus P.=. AT.QueueItemStatusDone]
    Just maintext -> do
      res2 <- lift $ Le.Python.cmdSpacyNer (Le.Python.CmdSpacyNerOpts maintext)
      Le.Article.BL.saveSpacyNer articleId res2
      let articlePleaseBigId :: ArticlePleaseBigId
          articlePleaseBigId = P.toSqlKey (P.fromSqlKey articleId)
      whenQueueItem $ \qi ->
        P.update (entityKey qi) [QueueStatus P.=. AT.QueueItemStatusPos]
      resPos <- lift $ Le.Python.cmdSpacyPos (Le.Python.CmdSpacyPosOpts maintext)
      P.update articlePleaseBigId [ArticlePleaseBigSpacyPos P.=. (Just resPos)]
      resSentiment <- runSentiment resPos
      P.update articlePleaseBigId [ArticlePleaseBigFasttextSentimentAmazon P.=. (Just resSentiment)]
      case mTitle of
        Just title -> do
          resNerTitle <- lift $ Le.Python.cmdSpacyNer (Le.Python.CmdSpacyNerOpts title)
          resPosTitle <- lift $ Le.Python.cmdSpacyPos (Le.Python.CmdSpacyPosOpts title)
          P.update
            articlePleaseBigId
            [ ArticlePleaseBigTitleSpacyNer P.=. Just resNerTitle,
              ArticlePleaseBigTitleSpacyPos P.=. Just resPosTitle
            ]
          resSentimentTitle <-
            lift $
              Le.Python.cmdFasttextSentimentAmazon
                (Le.Python.CmdFasttextSentimentAmazonOpts [title])
          P.update articlePleaseBigId [ArticlePleaseBigTitleFasttextSentimentAmazon P.=. (Just resSentimentTitle)]
        Nothing -> pure ()
      whenQueueItem $ \qi ->
        P.update (entityKey qi) [QueueStatus P.=. AT.QueueItemStatusDone]
  where
    runSentiment resPos = do
      let toks = Le.Python.cprTokens resPos
      let sentenceGroups = Data.List.groupBy (\_x y -> Le.Python.sptIsSentStart y /= Just True) toks
      let sentences = map (\sentence -> T.concat (map Le.Python.sptText sentence)) sentenceGroups
      let sentences2 = map (T.replace "\n" " ") sentences
      lift $ Le.Python.cmdFasttextSentimentAmazon (Le.Python.CmdFasttextSentimentAmazonOpts sentences2)
    whenQueueItem f =
      case mQueueItem of
        Nothing -> pure ()
        Just x -> f x

-- | Limited to latest N articles for now
reprocessDownloadedItems :: Le ()
reprocessDownloadedItems = do
  logInfo $ display $ ("> Beginning. Getting rows" :: Text)
  rows :: [(P.Single Int64, P.Single (Maybe Text))] <-
    runDb $
      E.rawSql
        [q|select id, title from article_please order by id desc limit ?|]
        [P.PersistInt64 100]
  logInfo $ display $ "> Got rows. Length: " <> tshow (length rows)
  speed <- Le.Speed.newSpeed (length rows)
  env <- ask
  pooledForConcurrentlyN_ (envNumCapabilities env) (zip [1 ..] rows) $ \(i, (P.Single articleId, P.Single mTitle)) -> do
    Le.Speed.withProgress i speed $ \t -> do
      logInfo $ display $ "> Progress: " <> t
    let articlePleaseBigId :: ArticlePleaseBigId
        articlePleaseBigId = P.toSqlKey articleId
    big <- mustFindME $ runDb $ P.get articlePleaseBigId
    runDb $ processDownloadedItem Nothing (P.toSqlKey articleId) mTitle (Just (articlePleaseBigMaintext big))
