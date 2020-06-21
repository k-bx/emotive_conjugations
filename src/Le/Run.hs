module Le.Run where

import qualified Data.String.Class as S
import Le.App
import qualified Le.CommonCrawl
import qualified Le.CommonCrawl.Cmd
import Le.Import
import qualified Le.Migrate
import qualified Le.Python
import qualified Le.Queue.Worker
import qualified Le.Shake
import qualified Le.WebApp
import qualified Le.WebApp.Dev
import qualified Le.WebApp.GenElm
import Options.Applicative
import System.Environment (withArgs)

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
    <> cmd "webapp" "Run web app" (pure (Le.WebApp.run (S.toText ver)))
    <> cmd "download-and-filter" "Run workers to filter data" (pure (run Le.CommonCrawl.Cmd.downloadAndFilter))
    <> cmd "test-download-and-filter" "Run workers to filter data" (pure (run Le.CommonCrawl.Cmd.testDownloadAndFilter))
    <> cmd "migrate" "Run migrations" (pure Le.Migrate.runMigrations)
    <> cmd "clean-db-data" "Clean some data" (pure Le.Migrate.cleanDbData)
    <> cmd "test-python-subcommand" "Ping python" (pure (run Le.Python.runTest))
    <> cmd "test-python-article" "Newspaper python library test" (pure (run Le.Python.parseArticleNewspaperTest))
    <> cmd "extract-articles" "Parse filtered articles via newspaper" (pure (run Le.CommonCrawl.Cmd.parseFilteredArticles))
    <> cmd "gen-elm" "Generate Api.elm" (pure Le.WebApp.GenElm.run)
    <> cmd "dev-webapp" "Run webapp listening to fs changes and rebuilding as needed" (pure Le.WebApp.Dev.main)
    <> cmd
      "shake"
      "Shake things up"
      (flip withArgs Le.Shake.shake <$> many (strArgument (metavar "-- Shake arguments")))
    <> cmd "spacy-ner-articles" "Run NER on articles" (pure (run Le.CommonCrawl.Cmd.spacyNerArticles))
    <> cmd "spacy-pos-articles" "Run POS on articles" (pure (run Le.CommonCrawl.Cmd.spacyPosArticles))
    <> cmd "queue-worker" "Consume the queue for a remote database" (pure (runQueue Le.Queue.Worker.main))
    <> cmd "queue-worker-local" "Like queue-worker but reads conj-queue-local.dhall" (pure (runQueueLocal Le.Queue.Worker.main))
    <> cmd "test-spacy-pos" "Test spacy POS" (pure (run Le.CommonCrawl.Cmd.testSpacyPos))
    <> cmd "test-fasttext-sentiment-amazon" "Test spacy POS" (pure (run Le.Python.testFasttextSentimentAmazon))

cmd :: String -> String -> Parser a -> Mod CommandFields a
cmd n d p = command n (info p (progDesc d))
