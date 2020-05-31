{-# LANGUAGE TemplateHaskell #-}

module Le.Python where

import Control.Lens ((.~))
import qualified Crypto.Hash
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BL
import qualified Data.String.Class as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Database.Persist.Postgresql as P
import Elm.Derive (deriveBoth)
import qualified Le.Config
import Le.Import
import Le.Util
import qualified Network.Wreq as W
import qualified UnliftIO.Process

newtype PythonError = PythonError String
  deriving (Show)

instance Exception PythonError

data CmdParseArticleOpts = CmdParseArticleOpts
  { cpaHtml :: Text,
    cpaUrl :: Text
  }
  deriving (Generic, Show)

deriveBoth (jsonOpts 3) ''CmdParseArticleOpts

data CmdParseArticleRes = CmdParseArticleRes
  { cprTitle :: Text,
    cprAuthors :: [Text],
    cprPubDate :: Maybe POSIXTime,
    cprDescription :: Text,
    cprText :: Text,
    cprLanguage :: Text
  }
  deriving (Generic, Show)

deriveBoth (jsonOpts 3) ''CmdParseArticleRes

data CmdSpacyNerOpts = CmdSpacyNerOpts
  { csnText :: Text
  }
  deriving (Generic, Show)

deriveBoth (jsonOpts 3) ''CmdSpacyNerOpts

data CmdSpacyNerResEnt = CmdSpacyNerResEnt
  { cseText :: Text,
    cseStart :: Int,
    cseStartChar :: Int,
    cseEnd :: Int,
    cseEndChar :: Int,
    cseLabel_ :: Text
  }
  deriving (Generic, Show, Eq)

deriveBoth (jsonOpts 3) ''CmdSpacyNerResEnt

data CmdSpacyNerRes = CmdSpacyNerRes
  { csrEnts :: [CmdSpacyNerResEnt]
  }
  deriving (Generic, Show, Eq)

deriveBoth (jsonOpts 3) ''CmdSpacyNerRes

instance P.PersistField CmdSpacyNerRes where
  toPersistValue = P.toPersistValueJSON

  fromPersistValue = P.fromPersistValueJSON

instance P.PersistFieldSql CmdSpacyNerRes where
  sqlType _ = P.SqlString

data CmdSpacyPosOpts = CmdSpacyPosOpts
  { cspText :: Text
  }
  deriving (Generic, Show)

deriveBoth (jsonOpts 3) ''CmdSpacyPosOpts

-- | Spacy's `Token` type encoded
data SpacyToken = SpacyToken
  { sptText :: Text,
    sptOrth :: Integer, -- ^ note: too big for Int
    sptLemma_ :: Text,
    sptPos_ :: Text,
    sptTag_ :: Text,
    sptDep_ :: Text,
    sptShape_ :: Text,
    sptIsAlpha :: Bool,
    sptIsAscii :: Bool,
    sptIsDigit :: Bool,
    sptIsPunct :: Bool,
    sptIsLeftPunct :: Bool,
    sptIsRightPunct :: Bool,
    sptIsSpace :: Bool,
    sptIsBracket :: Bool,
    sptIsQuote :: Bool,
    sptIsCurrency :: Bool,
    sptLikeUrl :: Bool,
    sptLikeNum :: Bool,
    sptLikeMail :: Bool,
    sptIsOov :: Bool,
    sptIsStop :: Bool,
    sptHeadI :: Int,
    sptLeftEdgeI :: Int,
    sptRightEdgeI :: Int,
    sptI :: Int,
    sptEntType_ :: Text,
    sptEntIob_ :: Text,
    sptEntKbId :: Int,
    sptEntKbId_ :: Text,
    sptNorm_ :: Text,
    sptLang_ :: Text,
    sptProb :: Double,
    sptIdx :: Int,
    sptSentiment :: Double,
    sptLexId :: Int,
    sptRank :: Int,
    sptCluster :: Int
  }
  deriving (Generic, Show, Eq)

deriveBoth (jsonOpts 3) ''SpacyToken

data CmdSpacyPosRes = CmdSpacyPosRes
  { cprTokens :: [SpacyToken]
  }
  deriving (Generic, Show, Eq)

deriveBoth (jsonOpts 3) ''CmdSpacyPosRes

instance P.PersistField CmdSpacyPosRes where
  toPersistValue = P.toPersistValueJSON

  fromPersistValue = P.fromPersistValueJSON

instance P.PersistFieldSql CmdSpacyPosRes where
  sqlType _ = P.SqlString

data CmdParseNewsPleaseOpts = CmdParseNewsPleaseOpts
  { cnpHtml :: Text,
    cnpUrl :: Text,
    cnpDownloadDate :: POSIXTime
  }
  deriving (Generic, Show)

deriveBoth (jsonOpts 3) ''CmdParseNewsPleaseOpts

data CmdParseNewsPleaseRes = CmdParseNewsPleaseRes
  { cnrAuthors :: [Text],
    cnrDateDownload :: Maybe POSIXTime,
    cnrDatePublish :: Maybe POSIXTime,
    cnrDateModify :: Maybe POSIXTime,
    cnrDescription :: Maybe Text,
    cnrFilename :: Text,
    cnrImageUrl :: Text,
    cnrLanguage :: Maybe Text,
    cnrLocalpath :: Maybe Text,
    cnrTitle :: Maybe Text,
    cnrTitlePage :: Maybe Text,
    cnrTitleRss :: Maybe Text,
    cnrSourceDomain :: Maybe Text,
    cnrMaintext :: Maybe Text,
    cnrUrl :: Text
  }
  deriving (Generic, Show)

deriveBoth (jsonOpts 3) ''CmdParseNewsPleaseRes

data CmdDownloadUrlNewsPleaseOpts = CmdDownloadUrlNewsPleaseOpts
  { cduUrl :: Text
  }
  deriving (Generic, Show)

deriveBoth (jsonOpts 3) ''CmdDownloadUrlNewsPleaseOpts

data Cmd
  = CmdPing
  | CmdParseArticle CmdParseArticleOpts
  | CmdParseNewsPlease CmdParseNewsPleaseOpts
  | CmdSpacyNer CmdSpacyNerOpts
  | CmdSpacyPos CmdSpacyPosOpts
  | CmdDownloadUrlNewsPlease CmdDownloadUrlNewsPleaseOpts
  deriving (Generic, Show)

deriveBoth (jsonOpts 0) ''Cmd

runPython :: Cmd -> Le (BL.ByteString, BL.ByteString)
runPython c = do
  tempDir <- asks envTempDir
  let cmdJson = J.encode c
  let sha = show (Crypto.Hash.hash (toBS cmdJson) :: Crypto.Hash.Digest Crypto.Hash.SHA256)
  let argsFp = tempDir <> "/conj-runpython-" <> sha
  liftIO $ BL.writeFile argsFp cmdJson
  -- (out, err) <-
  --   RIO.Process.proc
  --     Le.Config.pythonPath
  --     [ Le.Config.pythonScriptsDir <> "/main.py",
  --       argsFp
  --     ]
  --     $ \pconf -> do
  --       RIO.Process.readProcess_ pconf
  (ex, out, err) <-
    UnliftIO.Process.readProcessWithExitCode
      Le.Config.pythonPath
      [ Le.Config.pythonScriptsDir <> "/main.py",
        argsFp
      ]
      ""
  case ex of
    ExitFailure r ->
      throwIO $
        PythonError
          ( "Error while running a python subprocess: "
              <> show r
              <> "; "
              <> err
          )
    ExitSuccess -> do
      pure (S.fromString out, S.fromString err)

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

runPythonWeb :: FromJSON a => Cmd -> Le a
runPythonWeb c = do
  mgr <- asks envHttpManagerPython
  cfg <- asks envConfig
  let opts =
        W.defaults
          & W.manager .~ Right mgr
          & W.header "content-type" .~ ["application/json"]
  r <- liftIO $ W.postWith opts (S.toString (cfgPythonWebapp cfg <> "/api/call")) (J.encode c)
  pure (eitherErr (J.eitherDecode (r ^. W.responseBody)))

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
  let url = "https://www.nytimes.com/2020/03/29/arts/music/krzysztof-penderecki-dead.html"
  res <- cmdParseArticle (CmdParseArticleOpts html url)
  logInfo $ display $ "> Title: " <> tshow (cprTitle res)
  logInfo $ display $ "> Pub date: " <> tshow (fmap posixSecondsToUTCTime (cprPubDate res))

cmdParseArticle :: CmdParseArticleOpts -> Le CmdParseArticleRes
cmdParseArticle opts = runPythonParsing (CmdParseArticle opts)

cmdParseNewsPlease :: CmdParseNewsPleaseOpts -> Le CmdParseNewsPleaseRes
cmdParseNewsPlease opts = runPythonParsing (CmdParseNewsPlease opts)

cmdSpacyNer :: CmdSpacyNerOpts -> Le CmdSpacyNerRes
cmdSpacyNer opts = runPythonWeb (CmdSpacyNer opts)

cmdSpacyPos :: CmdSpacyPosOpts -> Le CmdSpacyPosRes
cmdSpacyPos opts = runPythonWeb (CmdSpacyPos opts)

cmdDownloadUrlNewsPlease :: CmdDownloadUrlNewsPleaseOpts -> Le CmdParseNewsPleaseRes
cmdDownloadUrlNewsPlease opts = runPythonParsing (CmdDownloadUrlNewsPlease opts)
