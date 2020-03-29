module Le.Run where

import qualified Data.String.Class as S
import Le.App
import qualified Le.CommonCrawl
import qualified Le.CommonCrawl.Cmd
import Le.Import
import qualified Le.Migrate
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
    <> cmd "ls-news-warcs" "List common crawl news warcs" (pure (run Le.CommonCrawl.listNewsWarcsCmd))
    <> cmd "webapp" "Run web app" (pure Le.WebApp.run)
    <> cmd "download-and-filter" "Run workers to filter data" (pure (run Le.CommonCrawl.Cmd.downloadAndFilter))
    <> cmd "test-download-and-filter" "Run workers to filter data" (pure (run Le.CommonCrawl.Cmd.testDownloadAndFilter))
    <> cmd "migrate" "Run migrations" (pure Le.Migrate.run)

cmd :: String -> String -> Parser a -> Mod CommandFields a
cmd n d p = command n (info p (progDesc d))
