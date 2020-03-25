module Le.AppUtils where

import qualified Data.String.Class as S
import GHC.Stack
import Le.Import
import Servant

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
