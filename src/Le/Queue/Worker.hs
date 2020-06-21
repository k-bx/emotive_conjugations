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
      logI <- askLogI
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
          let queueItemId = entityKey queueItem
          logI $ "> working with queue item: " <> tshow @Int64 (P.fromSqlKey queueItemId) <> "; status: " <> tshow (queueStatus (ev queueItem))
          logI $ "> downloading..." <> tshow @Int64 (P.fromSqlKey queueItemId) <> "; status: " <> tshow (queueStatus (ev queueItem))
          P.update queueItemId [QueueStatus P.=. AT.QueueItemStatusDownloading]
          res1 <-
            lift $
              Le.Python.cmdDownloadUrlNewsPlease $
                Le.Python.CmdDownloadUrlNewsPleaseOpts {cduUrl = queueUrl (ev queueItem)}
          P.update queueItemId [QueueStatus P.=. AT.QueueItemStatusExtracting]
          mArticleId <- lift $ Le.Article.BL.repsertReceivedParseNewsPleaseRes (queueUrl (ev queueItem)) Nothing Nothing i speed res1
          P.update queueItemId [QueueStatus P.=. AT.QueueItemStatusNer]
          articleId <- mustFindE mArticleId
          P.update queueItemId [QueueArticleId P.=. Just articleId]
          case Le.Python.cnrMaintext res1 of
            Nothing -> do
              P.update queueItemId [QueueStatus P.=. AT.QueueItemStatusDone]
            Just maintext -> do
              res2 <- lift $ Le.Python.cmdSpacyNer (Le.Python.CmdSpacyNerOpts maintext)
              lift $ Le.Article.BL.saveSpacyNer articleId res2
              let articlePleaseBigId :: ArticlePleaseBigId
                  articlePleaseBigId = P.toSqlKey (P.fromSqlKey articleId)
              P.update queueItemId [QueueStatus P.=. AT.QueueItemStatusPos]
              resPos <- lift $ Le.Python.cmdSpacyPos (Le.Python.CmdSpacyPosOpts maintext)
              P.update articlePleaseBigId [ArticlePleaseBigSpacyPos P.=. (Just resPos)]
              resSentiment <- runSentiment resPos
              P.update articlePleaseBigId [ArticlePleaseBigFasttextSentimentAmazon P.=. (Just resSentiment)]
              case Le.Python.cnrTitle res1 of
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
              P.update queueItemId [QueueStatus P.=. AT.QueueItemStatusDone]
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
        runSentiment resPos = do
          let toks = Le.Python.cprTokens resPos
          let sentenceGroups = Data.List.groupBy (\_x y -> Le.Python.sptIsSentStart y /= Just True) toks
          let sentences = map (\sentence -> T.concat (map Le.Python.sptText sentence)) sentenceGroups
          lift $ Le.Python.cmdFasttextSentimentAmazon (Le.Python.CmdFasttextSentimentAmazonOpts sentences)
