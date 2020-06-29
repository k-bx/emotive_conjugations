{-# LANGUAGE QuasiQuotes #-}

module Le.Article.Queries where

import qualified Data.List
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Database.Esqueleto
import qualified Database.Persist.Postgresql as P
import qualified Le.Config
import Le.Import hiding ((^.), isNothing, on)
import Le.Model
import Le.Util

queryPersonNamedEntities ::
  MonadIO m => Text -> Int -> ReaderT SqlBackend m [Text]
queryPersonNamedEntities query page = do
  case map T.toLower (T.words query ++ ["", "", ""]) of
    (w1 : w2 : w3 : _) -> do
      let offsetT = tshow @Int ((page - 1) * lim)
      let qry =
            [qc|
      SELECT DISTINCT proper FROM "named_propers" np
       WHERE ((np.search1 like ? or np.search2 like ? or np.search3 like ?)
             and (np.search1 like ? or np.search2 like ? or np.search3 like ?)
             and (np.search1 like ? or np.search2 like ? or np.search3 like ?)
            )
      LIMIT ${limT}
      OFFSET ${offsetT}
          |]
      let qus =
            map
              PersistText
              [ w1 <> "%",
                w1 <> "%",
                w1 <> "%",
                w2 <> "%",
                w2 <> "%",
                w2 <> "%",
                w3 <> "%",
                w3 <> "%",
                w3 <> "%"
              ]
      ents0 <- fmap (map unSingle) $ rawSql qry qus
      (namedPropersMap :: Map Text NamedPropers) <-
        P.selectList [NamedPropersEntity P.<-. ents0] []
          |> fmap (map (\e -> (unNamedPropersKey (entityKey e), ev e)))
          |> fmap M.fromList
      pure
        ( ents0
            |> map
              ( \ent ->
                  M.lookup ent namedPropersMap
                    |> fmap namedPropersProper
                    |> fromMaybe ent
              )
            |> nubSet
        )
    _ -> error "impossible!"
  where
    lim = Le.Config.entitiesPerPage
    limT = tshow @Int lim

queryPersonArticlesPlease ::
  MonadIO m => Maybe Text -> ReaderT SqlBackend m [Entity ArticlePlease]
queryPersonArticlesPlease mPerson = do
  case fromMaybe "" (T.strip <$> mPerson) of
    "" ->
      rawSql
        [qc|
select ??
from "article_please"
where "article_please"."date_publish" is not null
  and "article_please".title is not null
order by "article_please"."id" desc 
limit ${limT}
    |]
        []
-- order by "article_please"."date_publish" desc, "article_please"."id" desc 
    person -> do
      mProper <- rawSql [qc|select proper from named_propers where entity=?|] [PersistText person]
      entities <- case mProper of
        (Single proper : _) -> do
          rawSql [qc|select entity from named_propers where proper = ?|] [PersistText proper]
            & fmap (map unSingle)
        _ -> pure [person]
      let entitiesQuestions =
            "("
              <> ( T.concat
                     ( Data.List.intersperse
                         ","
                         (map (const "?") entities)
                     )
                 )
              <> ")"
          entitiesAnswers = map PersistText entities
      rawSql
        [qc|
select ??
from "article_please"
inner join named_entity ne on ne.article_please_id = "article_please".id
where ne.entity in ${entitiesQuestions}
  and "article_please".title is not null
group by ("article_please"."id")
order by "article_please"."date_publish" desc, "article_please"."id" desc 
limit ${limT}
              |]
        entitiesAnswers
  where
    lim = Le.Config.articlesLimit
    limT = tshow @Int lim
