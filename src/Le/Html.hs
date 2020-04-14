module Le.Html where

import qualified Data.Text as T
import Le.Import

splitHeadersAndHtml :: Text -> (Text, Text)
splitHeadersAndHtml t =
  let go headersAcc [] = (T.unlines (reverse headersAcc), "")
      go headersAcc (l : linesLeft) =
        if ":" `T.isInfixOf` l
          || "HTTP" `T.isInfixOf` l
          || T.strip l == ""
          then go (l : headersAcc) linesLeft
          else (T.unlines (reverse headersAcc), T.unlines linesLeft)
   in go [] (T.lines t)
