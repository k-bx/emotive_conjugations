module Le.AppUtils where

import qualified Data.Aeson as J
import qualified Data.String.Class as S
import qualified Database.Persist.Postgresql as P
import GHC.Stack
import Le.Import
import Servant
import qualified UnliftIO

-- | Safeguard. Good for GHC HasCallStack
sg ::
  (MonadUnliftIO m, HasCallStack) =>
  m a ->
  m a
sg act = catchAny act onErr
  where
    onErr e = do
      case fromException e of
        Just (ServerError {errHTTPCode = 302}) -> throwIO e
        _ -> do
          liftIO
            $ S.hPutStrLn stderr
            $ "Error: Safeguard caught an exception: " <> tshow e
              <> ". Call stack: "
              <> S.toText (prettyCallStack callStack)
          liftIO $ throwIO e

-- | Throws 404
mustFind :: Maybe a -> AppM a
mustFind = maybe notFound pure

-- | Throws 404
mustFindM :: AppM (Maybe a) -> AppM a
mustFindM = (=<<) mustFind

data ObjectNotFound = ObjectNotFound deriving (Show)

instance Exception ObjectNotFound

-- | Logs error
mustFindE :: MonadIO m => Maybe a -> m a
mustFindE = maybe (throwIO ObjectNotFound) pure

-- | Logs error via `throwIO`
mustFindME :: MonadIO m => m (Maybe a) -> m a
mustFindME = (=<<) mustFindE

notFound :: AppM a
notFound = throwM err404

logOnError :: Le () -> Le ()
logOnError act =
  UnliftIO.catchAny act onErr
  where
    onErr e = do
      logError $ display $ "> got error (continuing): " <> tshow e

formErrors :: [(Text, Text)] -> AppM a
formErrors xs = throwM err400 {errBody = J.encode xs}

withFormRes :: Validation [(Text, Text)] t -> (t -> AppM a) -> AppM a
withFormRes res f =
  case res of
    Success v -> f v
    Failure errs -> formErrors errs

-- | Useful for P.PersistField instances that reuse aeson for encoding/decoding of ADTs as strings
toPersistValueJSONText :: (ToJSON a, Show a) => a -> P.PersistValue
toPersistValueJSONText x =
  case J.toJSON x of
    (J.String s) -> P.PersistText s
    _ -> error $ "Couldn't encode as string: " <> show x

fromPersistValueJSONText :: (FromJSON a) => P.PersistValue -> Either Text a
fromPersistValueJSONText x = do
  v <- P.fromPersistValue x
  case v of
    P.PersistText t -> case J.fromJSON (J.String (S.fromText t)) of
      J.Error e -> Left (S.toText e)
      J.Success a -> Right a
    _ -> Left $ "Expected persist value as text: " <> tshow x
