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

reindexProper :: Le ()
reindexProper = do
  logInfo $ display $ ("> reindexProper"::Text)
  [P.Single lengthNamedEntities] <- Le.App.runDb $ E.rawSql [q|select count(*) from named_entity|] []
  -- nersRes <- Le.App.runDb $ P.selectSourceRes [NamedEntityProper P.==. Nothing] []
  nersRes <- Le.App.runDb $ P.selectSourceRes [] []
  speed <- Le.Speed.newSpeed lengthNamedEntities
  -- app <- ask
  -- pooledForConcurrentlyN_ (envNumCapabilities app) (zip [0 ..] ners) $ \(i, ner) -> do
  Le.App.forCondResEnum nersRes $ \(i, ner) -> do
    Le.Speed.withProgress i speed $ \t -> do
      logInfo $ display $ "> Processing ner: " <> t
    ensureNerProper ner

ensureNerProper :: Entity NamedEntity -> Le ()
ensureNerProper ner = do
  Le.App.runDb $ do
      mProp <- P.get (NamedPropersKey (namedEntityEntity (ev ner)))
      case mProp of
        Just _prop -> do
          pure ()
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
          -- P.update (entityKey ner) [NamedEntityProper P.=. Just mostPopularEntity]
          let (s1, s2, s3) = computeSearchTerms (namedEntityEntity (ev ner))
          E.rawExecute
            [q|insert into named_propers (entity, proper, search1, search2, search3, canonical) values (?, ?, ?, ?, ?, ?)
               on conflict do nothing|]
            [ P.PersistText (namedEntityEntity (ev ner))
            , P.PersistText mostPopularEntity
            , P.PersistText s1, P.PersistText s2, P.PersistText s3
            , P.PersistText (namedEntityCanonicalForm (namedEntityEntity (ev ner)))
            ]
          pure ()
