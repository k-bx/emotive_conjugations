{-# LANGUAGE QuasiQuotes #-}

module Le.Migrate where

import Control.Monad.Logger (runNoLoggingT)
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
    P.withPostgresqlPool (S.fromText (cfgPsqlConnString (appConfig app))) 1 $ \mpool -> do
      liftIO $ flip P.runSqlPool mpool $ migrateData

ensureIndexes :: ReaderT P.SqlBackend IO ()
ensureIndexes = do
  ensureIndex "workspace" "workspace_company_id_fkeyi" ["company_id"]
  ensureIndex "user" "user_company_id_fkeyi" ["company_id"]
  ensureIndex "employee" "employee_workspace_id_fkeyi" ["workspace_id"]
  ensureIndex "employee" "employee_workspace_id_fkeyi" ["workspace_id"]
  ensureIndex
    "notification"
    "notification_created_at_user_id_keyi"
    ["created_at", "user_id"]
  ensureIndex
    "employee_xvector"
    "employee_xvector_employee_id_fkeyi"
    ["employee_id"]
  ensureIndex "audio_chain" "audio_chain_workspace_id_fkeyi" ["workspace_id"]
  ensureIndex
    "audio_chain"
    "audio_chain_conversation_id_fkeyi"
    ["conversation_id"]
  ensureIndex
    "audio_chain_piece"
    "audio_chain_piece_audio_chain_id_fkeyi"
    ["audio_chain_id"]
  ensureIndex "report_ready" "report_ready_mic_fkeyi" ["mic"]
  ensureIndex "report_mc" "report_mc_report_ready_id_fkeyi" ["report_ready_id"]
  ensureIndex "report_mc" "report_mc_employee_id_fkeyi" ["employee_id"]
  ensureIndex
    "report_conversation"
    "report_conversation_report_mc_id_fkeyi"
    ["report_mc_id"]
  ensureIndex "rttmm" "rttmm_conversation_id_fkeyi" ["conversation_id"]
  ensureIndex "rttmm" "rttmm_time_keyi" ["time"]
  ensureIndex "nlp" "nlp_time_keyi" ["time"]
  ensureIndex "nlp" "nlp_conversation_id_keyi" ["conversation_id"]
  ensureIndex "nlp_keyword" "nlp_keyword_nlp_id_keyi" ["nlp_id"]
  ensureIndex "voice_sample" "voice_sample_employee_id_keyi" ["employee_id"]

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
  when (version <= 1) migration01
  -- Update this when you add more migrations
  when (version < 2) (setMigrationVersion 2)

latestVersion :: Int
latestVersion = 1

migration01 :: ReaderT P.SqlBackend IO ()
migration01 = do
  error "not implemented"

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
