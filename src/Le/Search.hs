{-# LANGUAGE QuasiQuotes #-}

module Le.Search where

import qualified Data.Char
import qualified Data.HashMap.Monoidal as MHM
import qualified Data.List
import qualified Data.Text as T
import qualified Database.Esqueleto as E
import qualified Database.Persist.Postgresql as P
import qualified Le.App
import Le.Import
import Le.Model
import qualified Le.Speed
import Le.Util
import qualified Safe

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
    removePrimeS =
      T.replace "'s" ""
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

reindexProper :: Le ()
reindexProper = do
  ners <- Le.App.runDb $ P.selectList [NamedEntityProper P.==. Nothing] []
  let overallLen = length ners
  speed <- Le.Speed.newSpeed overallLen
  app <- ask
  pooledForConcurrentlyN_ (appNumCapabilities app) (zip [0 ..] ners) $ \(i, ner) -> do
    Le.Speed.withProgress i speed $ \t -> do
      logInfo $ display $ "> Processing ner: " <> t
    Le.App.runDb $ do
      -- mNEProp <- P.getBy (UniqueNamedPropersEntity (namedEntityEntity (ev ner)))
      mNEProp <- P.get (NamedPropersKey (namedEntityEntity (ev ner)))
      case mNEProp of
        Just neProp -> do
          P.update (entityKey ner) [NamedEntityProper P.=. Just (namedPropersProper neProp)]
        Nothing -> do
          nersSameCanonical <- P.selectList [NamedEntityCanonical P.==. namedEntityCanonical (ev ner)] []
          let mostPopularEntity :: Text
              mostPopularEntity =
                nersSameCanonical
                  |> map (\x -> (namedEntityEntity (ev x), Sum (1 :: Int)))
                  |> MHM.fromListWith (<>)
                  |> MHM.toList
                  |> Data.List.sortBy (flip compare `on` snd)
                  |> Safe.headMay
                  |> fmap fst
                  |> fromMaybe (namedEntityEntity (ev ner))
          P.update (entityKey ner) [NamedEntityProper P.=. Just mostPopularEntity]
          E.rawExecute
            [q|insert into named_propers (entity, proper) values (?, ?)
               on conflict do nothing|]
            [P.PersistText (namedEntityEntity (ev ner)), P.PersistText mostPopularEntity]
          -- P.repsert
          --   (NamedPropersKey (namedEntityEntity (ev ner)))
          --   ( NamedPropers
          --       { namedPropersEntity = namedEntityEntity (ev ner),
          --         namedPropersProper = mostPopularEntity
          --       }
          --   )
          pure ()
