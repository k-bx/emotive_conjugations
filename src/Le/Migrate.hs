{-# LANGUAGE QuasiQuotes #-}

module Le.Migrate where

import qualified Conduit as C
import Conduit ((.|))
import qualified Data.String.Class as S
import qualified Data.Text as T
import Database.Esqueleto
import qualified Database.Esqueleto as E
import qualified Database.Persist.Postgresql as P
import qualified Le.App
import Le.AppUtils
import Le.Import
import Le.Model
import qualified Le.Search
import qualified Le.Speed
import qualified Prelude

runMigrations :: IO ()
runMigrations = do
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
        liftIO $ flip P.runSqlPool mpool $ migrateData app

ensureIndexes :: ReaderT P.SqlBackend IO ()
ensureIndexes = do
  -- ensureIndex "article_np" "article_np_date_i" ["date"]
  ensureIndex "article_please" "article_please_date_publish_i" ["date_publish"]
  ensureIndex "article" "article_warc_id_i" ["warc_id"]
  ensureIndex "named_entity" "named_entity_entity_i" ["entity"]
  ensureIndex "named_entity" "named_entity_label_search1_i" ["label_", "search1"]
  ensureIndex "named_entity" "named_entity_label_search2_i" ["label_", "search2"]
  ensureIndex "named_entity" "named_entity_label_search3_i" ["label_", "search3"]
  ensureIndex "named_entity" "named_entity_canonical_i" ["canonical"]
  ensureIndex "named_propers" "named_propers_proper_i" ["proper"]
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
            [qc|create index ${indexName} on public.${tableName} using btree (${cols})|]
      liftIO $ Prelude.putStrLn $ S.toString query
      rawExecute query []
    (_ :: [P.Single Text]) -> pure ()

migrateData :: App -> ReaderT P.SqlBackend IO ()
migrateData app = do
  migrationInfo <- getMigrationInfo
  let version = migrationInfoVersion migrationInfo
  when (version <= 1) Le.Search.reindexNers -- 1 -> 2
  when (version <= 2) (runRIO app Le.Search.reindexProper) -- 2 -> 3
  when (version <= 3) (pure ()) -- 3-> 4
  when (version < latestVersion) (setMigrationVersion latestVersion)

-- Update this when you add more migrations
latestVersion :: Int
latestVersion = 4

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

migrate03 :: App -> ReaderT P.SqlBackend IO ()
migrate03 app = pure ()
  where
    _legacy = do
      runRIO app $ logInfo $ display $ ("> starting migrate03" :: Text)
      [Single lengthArticlePleases] <- E.rawSql [q|select count(*) from article_please|] []
      runRIO app $ logInfo $ display $ ("> length: " <> tshow lengthArticlePleases)
      speed <- Le.Speed.newSpeed lengthArticlePleases
      articlesRes <- selectSourceRes ([] :: [P.Filter ArticlePlease]) []
      C.withAcquire articlesRes $ \src ->
        C.runConduit $
          (C.getZipSource ((,) <$> C.ZipSource (C.yieldMany [0 ..]) <*> C.ZipSource src))
            .| C.mapM_C (act speed)
      where
        act :: Le.Speed.Speed -> (Int, Entity ArticlePlease) -> ReaderT P.SqlBackend IO ()
        act speed (i, _articlePlease) = do
          Le.Speed.withProgress i speed $ \t -> do
            runRIO app $ logInfo $ display $ "> Processing article please: " <> t
          -- void $ P.insert $ undefined
          -- ArticlePleaseBig
          --   { articlePleaseBigMaintext = articlePleaseMaintext (ev articlePlease),
          --     articlePleaseBigSpacyNer = articlePleaseSpacyNer (ev articlePlease),
          --     articlePleaseBigSpacyPos = articlePleaseSpacyPos (ev articlePlease)
          --   }
          pure ()
