module Le.Run where

import qualified Data.String.Class as S
import qualified Dhall
import qualified Le.CommonCrawl
import Le.Import
import qualified Network.AWS as AWS
import Options.Applicative
import RIO.Process
import qualified System.Directory

main :: String -> IO ()
main ver = join (customExecParser (prefs (showHelpOnError <> showHelpOnEmpty)) (info (helper <*> hsubparser (commands ver)) imod))

imod :: InfoMod a
imod = fullDesc <> progDesc "Emotive Conjugations Project"

commands :: String -> Mod CommandFields (IO ())
commands ver =
  mempty
    <> cmd "version" "Show version" (pure (run $ logInfo $ "Version " <> display (S.toText ver)))
    <> cmd "extract-example-warc" "Extract example warc" (pure (run Le.CommonCrawl.extractExampleWarc))
    <> cmd "ls-news-warcs" "List common crawl news warcs" (pure (run Le.CommonCrawl.listNewsWarcs))

cmd :: String -> String -> Parser a -> Mod CommandFields a
cmd n d p = command n (info p (progDesc d))

run :: RIO App a -> IO a
run act = withApp $ \env -> runRIO env act

withApp :: (App -> IO a) -> IO a
withApp f = do
  cfg <- liftIO readConfig
  lo <- logOptionsHandle stderr False
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf -> do
    awsEnv <- AWS.newEnv AWS.Discover
    let app = App
          { appLogFunc = lf,
            appProcessContext = pc,
            appConfig = cfg,
            appAwsEnv = awsEnv
          }
    f app

readConfig :: IO Config
readConfig = do
  h <- System.Directory.getHomeDirectory
  Dhall.input Dhall.auto (S.toText (h <> "/conj.dhall"))
