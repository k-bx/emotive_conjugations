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

main :: IO ()
main = do
  lastSignalMV <- MVar.newEmptyMVar
  forever $ do
    mLastSignal <- MVar.tryReadMVar lastSignalMV
    Prelude.putStrLn ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
    c <-
      case mLastSignal of
        Nothing -> do
          Prelude.putStrLn $ ">> NOTHING"
          runProcess makeCmd
        Just ev ->
          case getEvPath ev of
            Nothing -> do
              Prelude.putStrLn $ ">> PATH IS NOTHING"
              runProcess makeCmd
            Just path ->
              let x' = S.toText path
               in if ".elm" `T.isSuffixOf` x'
                    then do
                      Prelude.putStrLn $ ">> elm"
                      runProcess makeElm
                    else
                      if ".scss" `T.isSuffixOf` x'
                        then do
                          Prelude.putStrLn $ ">> sass"
                          runProcess makeSass
                        else do
                          Prelude.putStrLn $ ">> NOMATCH"
                          runProcess makeCmd
    mWebApp <-
      case c of
        ExitSuccess -> do
          Just
            <$> startProcess runCmd
        _ -> pure Nothing
    Prelude.putStrLn $ ">> mWebApp: " ++ show mWebApp
    (mynotifywait lastSignalMV) >> threadDelay 10000 -- 10 ms
    case mWebApp of
      Just webApp -> do
        Prelude.putStrLn ">> Killing the webapp...."
        stopProcess webApp
      Nothing -> pure ()
    Prelude.putStrLn "==================================================="

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
