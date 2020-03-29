module Le.Python where

import qualified Le.Config
import Le.Import
import qualified RIO.Process

-- | Test connection and libaries
runTest :: Le ()
runTest = do
  let cmdJson = "{\"cmd\": \"ping\"}"
  (out, err) <-
    RIO.Process.proc
      Le.Config.pythonPath
      [ Le.Config.pythonScriptsDir <> "/main.py",
        cmdJson
      ]
      $ \pconf -> do
        RIO.Process.readProcess_ pconf
  case err of
    "pong\n" -> do
      logInfo "Test successful"
    _ -> do
      logError $ display $ "Test failed. out: " <> tshow out <> "; err: " <> tshow err
