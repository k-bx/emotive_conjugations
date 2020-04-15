{-# LANGUAGE QuasiQuotes #-}

module Le.Migrate where

import qualified Data.String.Class as S
import qualified Data.Text as T
import Database.Esqueleto
import qualified Database.Persist.Postgresql as P
import qualified Le.App
import Le.AppUtils
import Le.Import
import Le.Model
import Text.InterpolatedString.Perl6 (q, qc)
import qualified Prelude

run :: IO ()
run = do
  Le.App.withApp $ \app -> do
    runNoLoggingT
      $ P.withPostgresqlPool (S.fromText (cfgPsqlConnString (appConfig app))) 1
      $ \mpool -> do
        liftIO $ flip P.runSqlPool mpool $ P.runMigration migrateAll
        liftIO $ flip P.runSqlPool mpool $ ensureIndexes
    runNoLoggingT
      $ P.withPostgresqlPool
        (S.fromText (cfgPsqlConnString (appConfig app)))
        1
      $ \mpool -> do
        liftIO $ flip P.runSqlPool mpool $ migrateData

ensureIndexes :: ReaderT P.SqlBackend IO ()
ensureIndexes = do
  -- ensureIndex "article_np" "article_np_date_i" ["date"]
  ensureIndex "article_please" "article_please_date_publish_i" ["date_publish"]
  ensureIndex "article" "article_warc_id_i" ["warc_id"]
  pure ()

ensureIndex :: Text -> Text -> [Text] -> ReaderT P.SqlBackend IO ()
ensureIndex tableName indexName fieldNames = do
  res <-
    rawSql
      [q|
select indexdef from pg_indexes
where schemaname = 'public'
  and tablename = ?
  and indexname = ?
         |]
      [PersistText tableName, PersistText indexName]
  case res of
    [] -> do
      let cols = T.intercalate ", " fieldNames
      let query =
            [qc|create index {indexName} on public.{tableName} using btree ({cols})|]
      liftIO $ Prelude.putStrLn $ S.toString query
      rawExecute query []
    (_ :: [P.Single Text]) -> pure ()

migrateData :: ReaderT P.SqlBackend IO ()
migrateData = do
  migrationInfo <- getMigrationInfo
  let version = migrationInfoVersion migrationInfo
  when (version <= 1) migration01 -- 1 -> 2
  when (version <= 2) migration02 -- 2 -> 3
  when (version <= 3) migration02 -- 3 -> 4
  when (version < latestVersion) (setMigrationVersion latestVersion)

-- Update this when you add more migrations
latestVersion :: Int
latestVersion = 4

migration01 :: ReaderT P.SqlBackend IO ()
migration01 = do
  pure ()

migration02 :: ReaderT P.SqlBackend IO ()
migration02 = do
  -- articlesNp <- P.selectList [] [P.Desc ArticleNpId]
  -- forM_ articlesNp $ \articleNp -> do
  --   let articleId = P.toSqlKey (P.fromSqlKey (entityKey articleNp))
  --   P.repsert
  --     articleId
  --     ( Article
  --         { articleUrl = articleNpUrl (ev articleNp),
  --           articleHost = articleNpHost (ev articleNp)
  --         }
  --     )
  pure ()

getMigrationInfo :: ReaderT P.SqlBackend IO MigrationInfo
getMigrationInfo = do
  res <- P.selectList [] []
  case res of
    [] -> do
      miId <- P.insert $ MigrationInfo {migrationInfoVersion = 1}
      mustFindMErr $ P.get miId
    [mi] -> pure $ ev mi
    _ -> error "Impossible! More than one migration info"

setMigrationVersion :: Int -> ReaderT P.SqlBackend IO ()
setMigrationVersion n = do
  P.updateWhere [] [MigrationInfoVersion P.=. n]

cleanDbData :: IO ()
cleanDbData = Le.App.run $ do
  -- Le.App.runDb $ P.deleteCascadeWhere ([] :: [P.Filter ArticleNp])
  Le.App.runDb $ P.deleteCascadeWhere ([] :: [P.Filter ArticlePlease])
  Le.App.runDb $ P.deleteCascadeWhere ([] :: [P.Filter Article])
