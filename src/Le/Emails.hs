{-# LANGUAGE QuasiQuotes #-}

module Le.Emails where

import qualified Data.String.Class as S
import Le.Import
import qualified Mail.Hailgun as Hailgun
import Servant

sendEmail :: Hailgun.MessageSubject -> Text -> Text -> Text -> AppM ()
sendEmail title txt html emailTo = do
  env <- ask
  let ctx =
        Hailgun.HailgunContext
          { hailgunDomain = S.toString (cfgMailgunDomain (appConfig env)),
            hailgunApiKey = S.toString (cfgMailgunApiKey (appConfig env)),
            hailgunProxy = Nothing
          }
  let hailgunMsg =
        Hailgun.hailgunMessage
          title
          (Hailgun.TextAndHTML (toBS txt) (toBS html))
          "meetup.events.info@gmail.com"
          ( Hailgun.MessageRecipients
              { recipientsTo = [S.fromText emailTo],
                recipientsCC = [],
                recipientsBCC = []
              }
          )
          []
  case hailgunMsg of
    Left err -> do
      let e = "Error while constructing Hailgun email: " <> (fromString err)
      logError e
      throwM $ err500 {errBody = S.toLazyByteString (utf8BuilderToText e)}
    Right msg -> do
      liftIO (Hailgun.sendEmail ctx msg) >>= \case
        Left err -> do
          let e =
                "Error while sending Hailgun email: "
                  <> display (S.toText (Hailgun.herMessage err))
          logError e
          throwM $ err500 {errBody = S.toLazyByteString (utf8BuilderToText e)}
        Right sendRsp -> do
          logInfo $
            "Email sent. Mailgun response: "
              <> fromString (Hailgun.hsrMessage sendRsp)

signInCodeTitle :: Text
signInCodeTitle = "Meetup Sign-In Link"

signInCodeTxt :: Text -> Text
signInCodeTxt code =
  [qc|
          Meowdy, partner

Your sign-in code is: ${code}

All the best
|]

signInCodeHtml :: Text -> Text
signInCodeHtml code =
  [qc|Meowdy, partner<br><br>

Your sign-in code is: ${code}<br><br>

All the best
|]
