{-# LANGUAGE QuasiQuotes #-}
module Le.Html where

import qualified Data.ByteString as B
import qualified Data.Char
import qualified Data.String.Class as S
import qualified Data.Text as T
import Le.Import

splitHeadersAndHtml :: B.ByteString -> (Text, Text)
splitHeadersAndHtml fullBs =
  let go headersAcc bsLeft | B.null bsLeft = (T.unlines (reverse headersAcc), S.toText bsLeft)
      go headersAcc bsLeft =
        let (line, rest0) = B.span (/= (fromIntegral (Data.Char.ord '\n'))) bsLeft
            rest1 = B.drop 1 rest0
            l = S.toText line
         in if (":" `T.isInfixOf` l
              || "HTTP" `T.isInfixOf` l
              || T.strip l == "")
               && (not ("<html" `T.isInfixOf` (T.toLower l)))
              then go (l : headersAcc) rest1
              else (T.unlines (reverse headersAcc), S.toText l <> "\n" <> S.toText rest1)
   in go [] fullBs

-- splitHeadersAndHtml :: Text -> (Text, Text)
-- splitHeadersAndHtml t =
--   let go headersAcc [] = (T.unlines (reverse headersAcc), "")
--       go headersAcc (l : linesLeft) =
--         if ":" `T.isInfixOf` l
--           || "HTTP" `T.isInfixOf` l
--           || T.strip l == ""
--           then go (l : headersAcc) linesLeft
--           else (T.unlines (reverse headersAcc), T.unlines linesLeft)
--    in go [] (T.lines t)

ex01 :: B.ByteString
ex01 = [q|
HTTP/1.1 200 OK
Date: Tue, 10 Mar 2020 15:35:22 GMT
Content-Type: text/html
X-Crawler-Content-Length: 22946
Content-Length: 89071
Connection: keep-alive
Keep-Alive: timeout=120
nel: {"report_to":"default","max_age":2592000,"include_subdomains":true,"failure_fraction":0.01}
X-Cache-Action: MISS
Vary: accept-encoding
X-Cache-Age: 0
Cache-Control: private, max-age=0, must-revalidate
X-Crawler-Content-Encoding: gzip
report-to: {"group":"default","max_age":2592000,"endpoints":[{"url":"https://europe-west1-bbc-otg-traf-mgr-bq-prod-4591.cloudfunctions.net/report-endpoint","priority":1}],"include_subdomains":true}
Etag: "d04acf55bae8d88284c4ff1a53bbd9ba"
X-LB-NoCache: true
X-PAL-Host: pal829.back.live.lbh.local:80
Via: 1.1 BBC-GTM
X-BBC-Edge-Cache-Status: MISS
X-BBC-Origin-Response-Status: 200
Server: BBC-GTM

<!DOCTYPE html> <html class="" lang="en-GB" > <head> <!-- Barlesque 5.0.0 --> <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" /> <meta name="description" content="The BBC artist page for Sébastien Martel. Find the best clips, watch programmes, catch up on the news, and read the latest Sé>
<!--[if (gt IE 8) | (IEMobile)]><!-->
<link rel="stylesheet" href="https://static.bbc.co.uk/frameworks/barlesque/5.0.0/orb/4/style/orb.min.css">
<!--<![endif]-->

<!--[if (lt IE 9) & (!IEMobile)]>
<link rel="stylesheet" href="https://static.bbc.co.uk/frameworks/barlesque/5.0.0/orb/4/style/orb-ie.min.css">
<![endif]-->
|]
