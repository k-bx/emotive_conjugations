module Le.Python where

import qualified Crypto.Hash
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BL
import qualified Data.String.Class as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Le.Config
import Le.Import
import qualified RIO.Process

data Cmd
  = CmdPing
  | CmdParseArticle CmdParseArticleOpts
  deriving (Generic, Show)

instance J.ToJSON Cmd where

  toEncoding = J.genericToEncoding (jsonOpts 0)

  toJSON = J.genericToJSON (jsonOpts 0)

instance J.FromJSON Cmd where
  parseJSON = J.genericParseJSON (jsonOpts 0)

data CmdParseArticleOpts
  = CmdParseArticleOpts
      { cpaHtml :: Text
      }
  deriving (Generic, Show)

instance J.ToJSON CmdParseArticleOpts where

  toEncoding = J.genericToEncoding (jsonOpts 3)

  toJSON = J.genericToJSON (jsonOpts 3)

instance J.FromJSON CmdParseArticleOpts where
  parseJSON = J.genericParseJSON (jsonOpts 3)

data CmdParseArticleRes
  = CmdParseArticleRes
      { cprTitle :: Text,
        cprAuthors :: [Text],
        cprPubDate :: Maybe POSIXTime,
        cprDescription :: Text,
        cprText :: Text,
        cprLanguage :: Text
      }
  deriving (Generic, Show)

instance J.ToJSON CmdParseArticleRes where

  toEncoding = J.genericToEncoding (jsonOpts 3)

  toJSON = J.genericToJSON (jsonOpts 3)

instance J.FromJSON CmdParseArticleRes where
  parseJSON = J.genericParseJSON (jsonOpts 3)

runPython :: Cmd -> Le (BL.ByteString, BL.ByteString)
runPython c = do
  tempDir <- asks appTempDir
  let cmdJson = J.encode c
  let sha = show (Crypto.Hash.hash (toBS cmdJson) :: Crypto.Hash.Digest Crypto.Hash.SHA256)
  let argsFp = tempDir <> "/conj-runpython-" <> sha
  liftIO $ BL.writeFile argsFp cmdJson
  (out, err) <-
    RIO.Process.proc
      Le.Config.pythonPath
      [ Le.Config.pythonScriptsDir <> "/main.py",
        argsFp
      ]
      $ \pconf -> do
        RIO.Process.readProcess_ pconf
  pure (out, err)

-- | Expect stdout to be a valid 'a' JSON
runPythonParsing :: FromJSON a => Cmd -> Le a
runPythonParsing c = do
  (out, err) <- runPython c
  case J.eitherDecode out of
    Right v -> pure v
    Left e -> do
      logError $ display $
        "Python command failed. Command: " <> T.take 300 (tshow c)
          <> "; \nerror: "
          <> tshow e
          <> "; \nout: "
          <> T.take 300 (tshow out)
          <> "; \nerr: "
          <> S.toText err
      error "runPythonParsing failure"

-- | Test connection and libaries
runTest :: Le ()
runTest = do
  (out, err) <- runPython CmdPing
  case out of
    "pong\n" -> do
      logInfo "Test successful"
    _ -> do
      logError $ display $ "Test failed. out: " <> tshow out <> "; err: " <> tshow err

parseArticleNewspaperTest :: Le ()
parseArticleNewspaperTest = do
  -- https://www.nytimes.com/2020/03/29/arts/music/krzysztof-penderecki-dead.html
  html <- liftIO $ T.readFile "/home/kb/workspace/emotive_conjugations/data/test/penderecki-dead.html"
  res <- runPythonParsing (CmdParseArticle (CmdParseArticleOpts html))
  logInfo $ display $ "> Title: " <> tshow (cprTitle res)
  logInfo $ display $ "> Pub date: " <> tshow (fmap posixSecondsToUTCTime (cprPubDate res))
