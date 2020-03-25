module Le.Run where

import qualified Data.String.Class as S
import qualified Le.CommonCrawl
import Le.Import
import qualified Le.WebApp
import Options.Applicative

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
    <> cmd "webapp" "Run web app" (pure Le.WebApp.run)

cmd :: String -> String -> Parser a -> Mod CommandFields a
cmd n d p = command n (info p (progDesc d))

run :: RIO App a -> IO a
run act = withApp $ \env -> runRIO env act
