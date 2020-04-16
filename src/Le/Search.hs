module Le.Search where

import qualified Data.Char
import qualified Data.List
import qualified Data.Text as T
import qualified Database.Persist.Postgresql as P
import Le.Import
import Le.Model
import Le.Util

-- | Used for named entities indexing
computeSearchTerms :: Text -> (Text, Text, Text)
computeSearchTerms t =
  let wrds :: [Text]
      wrds =
        t |> T.words
          |> Data.List.sortBy (flip compare `on` (\x -> (T.length x, x)))
          |> take 3
          |> map (T.toLower . T.strip)
   in case wrds of
        [] -> ("", "", "")
        [w1] -> (w1, "", "")
        [w1, w2] -> (w1, w2, "")
        (w1 : w2 : w3 : _) -> (w1, w2, w3)

namedEntityCanonicalForm :: Text -> Text
namedEntityCanonicalForm t =
  let (s1, s2, s3) = computeSearchTerms t
   in [s1, s2, s3]
        |> map (stripNonAlpha . removePrimeS)
        |> Data.List.intersperse "-"
        |> T.concat
  where
    stripNonAlpha = T.filter Data.Char.isLetter
    -- `elon musk's` -> `elon musk`
    removePrimeS = T.replace "'s" ""
                   . T.replace "’s" ""

reindexNers :: ReaderT P.SqlBackend IO ()
reindexNers = do
  ners <- P.selectList [] []
  forM_ ners $ \ner -> do
    let (s1, s2, s3) = computeSearchTerms (namedEntityEntity (ev ner))
    P.update
      (entityKey ner)
      [ NamedEntitySearch1 P.=. Just s1,
        NamedEntitySearch2 P.=. Just s2,
        NamedEntitySearch3 P.=. Just s3,
        NamedEntityCanonical
          P.=. Just
            ( namedEntityCanonicalForm
                (namedEntityEntity (ev ner))
            )
      ]
