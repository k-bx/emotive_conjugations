module Le.Run where

import Le.Import
import Options.Applicative.Simple
import RIO.Process

main :: String -> IO ()
main ver = do
  (options, ()) <-
    simpleOptions
      ver
      "Header for command line arguments"
      "Program description, also for command line arguments"
      ( Options
          <$> switch
            ( long "verbose"
                <> short 'v'
                <> help "Verbose output?"
            )
      )
      empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf,
            appProcessContext = pc,
            appOptions = options
          }
     in runRIO app run

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
