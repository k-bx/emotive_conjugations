{-# LANGUAGE QuasiQuotes #-}

module Le.Migrate where

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
      $ P.withPostgresqlPool (S.fromText (cfgPsqlConnString (envConfig app))) 1
      $ \mpool -> do
        liftIO $ flip P.runSqlPool mpool $ P.runMigration migrateAll
        liftIO $ flip P.runSqlPool mpool $ ensureIndexes
    runNoLoggingT
      $ P.withPostgresqlPool
        (S.fromText (cfgPsqlConnString (envConfig app)))
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

migrateData :: Env -> ReaderT P.SqlBackend IO ()
migrateData app = do
  migrationInfo <- getMigrationInfo
  let version = migrationInfoVersion migrationInfo
  -- when (version <= 1) Le.Search.reindexNers -- 1 -> 2
  when (version <= 1) (pure ()) -- 1 -> 2
  when (version <= 2) (pure ()) -- 2 -> 3
  when (version <= 3) (pure ()) -- 3 -> 4
  when (version <= 4) (runRIO app Le.Search.reindexProper) -- 4 -> 5
  when (version < latestVersion) (setMigrationVersion latestVersion)

-- delete from named_propers;
-- runRIO app Le.Search.reindexProper
-- drop search1,2,3,proper from ner
-- alter table named_entity drop column search1;
-- alter table named_entity drop column search2;
-- alter table named_entity drop column search3;
-- alter table named_entity drop column proper;

-- Update this when you add more migrations
latestVersion :: Int
latestVersion = 5

getMigrationInfo :: ReaderT P.SqlBackend IO MigrationInfo
getMigrationInfo = do
  res <- P.selectList [] []
  case res of
    [] -> do
      miId <- P.insert $ MigrationInfo {migrationInfoVersion = 1}
      mustFindME $ P.get miId
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

recalc_canonical :: Env -> ReaderT P.SqlBackend IO ()
recalc_canonical app = do
  runRIO app $ logInfo $ display $ ("> starting recalc_canonical" :: Text)
  [Single lengthNers] <- E.rawSql [q|select count(*) from named_entity|] []
  runRIO app $ logInfo $ display $ ("> length: " <> tshow lengthNers)
  speed <- Le.Speed.newSpeed lengthNers
  nersRes <- selectSourceRes ([] :: [P.Filter NamedEntity]) []
  Le.App.forCondResEnum nersRes (act speed)
  where
    act :: Le.Speed.Speed -> (Int, Entity NamedEntity) -> ReaderT P.SqlBackend IO ()
    act speed (i, ner) = do
      Le.Speed.withProgress i speed $ \t -> do
        runRIO app $ logInfo $ display $ "> Processing ner: " <> t
      P.update (entityKey ner) [NamedEntityCanonical P.=. Just (Le.Search.namedEntityCanonicalForm (namedEntityEntity (ev ner)))]
      pure ()
