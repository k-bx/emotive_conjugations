module Le.WebApp.Dev where

import qualified Control.Concurrent.MVar as MVar
import qualified Data.String.Class as S
import qualified Data.Text as T
import RIO
import System.Directory (canonicalizePath)
import qualified System.FSNotify as FSNotify
import System.Info
import System.Process.Typed
import qualified Prelude

makeCmd :: ProcessConfig () () ()
makeCmd = "make dev"

makeElm :: ProcessConfig () () ()
makeElm = "make elm-dev && make elm-post"

makeSass :: ProcessConfig () () ()
makeSass = "make sass && make elm-post"

runCmd :: ProcessConfig () () ()
runCmd = case os of
  "darwin" ->
    -- https://github.com/haskell/process/issues/171
    setWorkingDir "dist" (proc "./dist/conj" ["webapp"])
  _ ->
    setWorkingDir "dist" (proc "./conj" ["webapp"])

reloadStaticEndpoint :: ProcessConfig () () ()
reloadStaticEndpoint = "curl --insecure -XPOST https://localhost:8081/api/dev/reload-static.json"

data RebuildPart = PartWhole | PartElm | PartScss

whatChanged :: Maybe FSNotify.Event -> RebuildPart
whatChanged mLastSignal =
  case mLastSignal of
    Nothing -> PartWhole
    Just ev ->
      case getEvPath ev of
        Nothing -> PartWhole
        Just path ->
          let pathT = S.toText path
           in if ".elm" `T.isSuffixOf` pathT
                then PartElm
                else
                  if ".scss" `T.isSuffixOf` pathT
                    then PartScss
                    else PartWhole

main :: IO ()
main = do
  lastSignalMV <- MVar.newEmptyMVar
  webapps <- newTVarIO []
  forever $ do
    mLastSignal <- MVar.tryReadMVar lastSignalMV
    Prelude.putStrLn ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
    exitStatus <-
      rebuild mLastSignal
    needsKillAndRestart lastSignalMV >>= \case
      False -> do
        Prelude.putStrLn $ ">> not starting the webapp (should be running already)"
      True -> do
        case exitStatus of
          ExitSuccess -> do
            p <- startProcess runCmd
            atomically $ modifyTVar webapps (p :)
          _ -> pure ()
    (mynotifywait lastSignalMV) >> threadDelay 10000 -- 10 ms
    needsKillAndRestart lastSignalMV >>= \case
      True -> tryStopWebapps =<< readTVarIO webapps
      False -> Prelude.putStrLn $ ">> not killing the webapp"
    Prelude.putStrLn "==================================================="

tryStopWebapps :: [Process stdin stdout stderr] -> IO ()
tryStopWebapps webApps = do
  case webApps of
    [] -> pure ()
    _ -> do
      Prelude.putStrLn ">> Killing the webapp...."
      mapM_ stopProcess webApps

needsKillAndRestart :: MVar FSNotify.Event -> IO Bool
needsKillAndRestart lastSignalMV = do
  fmap whatChanged (MVar.tryReadMVar lastSignalMV) >>= \case
    PartWhole -> pure True
    PartElm -> pure False
    PartScss -> pure False

rebuild :: Maybe FSNotify.Event -> IO ExitCode
rebuild mLastSignal =
  case whatChanged mLastSignal of
    PartWhole -> runProcess makeCmd
    PartElm -> do
      rv <- runProcess makeElm
      -- void $ runProcess reloadStaticEndpoint
      pure rv
    PartScss -> do
      rv <- runProcess makeSass
      -- void $ runProcess reloadStaticEndpoint
      pure rv

getEvPath :: FSNotify.Event -> Maybe FilePath
getEvPath ev =
  case ev of
    FSNotify.Modified mpath _t _bool -> Just mpath
    FSNotify.Added mpath _t _bool -> Just mpath
    FSNotify.Unknown mpath _t _bool -> Just mpath
    _ -> Nothing

-- | Similar to inoitfywait but hardcoded for our setup and is
-- cross-platform (should work on macOS). Waits for files to change
-- (excluding some) and then exits.
mynotifywait :: MVar FSNotify.Event -> IO ()
mynotifywait lastSignal = do
  dir <- canonicalizePath "."
  exit <- MVar.newEmptyMVar
  exit2 <- MVar.newEmptyMVar
  let pathMatches x =
        let x' = S.toText x
         in if (S.toText dir) `T.isPrefixOf` x'
              && not ("#" `T.isInfixOf` x')
              && not (".git" `T.isInfixOf` x')
              && not (".stack-work" `T.isInfixOf` x')
              && not (".shake" `T.isInfixOf` x')
              && not ("/dist/" `T.isInfixOf` x')
              then True
              else False
  Prelude.putStrLn $ "> Listening to directory: " <> dir
  _ <-
    FSNotify.withManager $ \mgr -> do
      let predicate ev =
            case getEvPath ev of
              Nothing -> False
              Just path -> pathMatches path
      _ <-
        FSNotify.watchTree mgr dir predicate $ \ev -> do
          Prelude.putStrLn $ "> FSNotify signal: " <> show ev
          MVar.isEmptyMVar lastSignal >>= \case
            False -> void $ swapMVar lastSignal ev
            True -> putMVar lastSignal ev
          MVar.putMVar exit2 ()
          MVar.putMVar exit ()
      MVar.takeMVar exit2
  MVar.takeMVar exit
