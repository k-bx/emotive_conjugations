{-# LANGUAGE QuasiQuotes #-}

module Le.Article.Queries where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Database.Esqueleto
import qualified Database.Persist.Postgresql as P
import qualified Le.Config
import Le.Import hiding ((^.), isNothing, on)
import Le.Model
import Le.Util
import Text.InterpolatedString.Perl6 (qc)

queryPersonNamedEntities ::
  MonadIO m => Text -> Int -> ReaderT SqlBackend m [Text]
queryPersonNamedEntities query page = do
  case map T.toLower (T.words query ++ ["", "", ""]) of
    (w1 : w2 : w3 : _) -> do
      let q =
            [qc|
      SELECT proper FROM "named_entity" ne
      WHERE label_ = 'PERSON'
        AND ((ne.search1 like ? or ne.search2 like ? or ne.search3 like ?)
             and (ne.search1 like ? or ne.search2 like ? or ne.search3 like ?)
             and (ne.search1 like ? or ne.search2 like ? or ne.search3 like ?)
            )
      GROUP BY ne.proper
      LIMIT {lim}
      OFFSET {(page - 1) * lim}
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
      ents0 <- fmap (map unSingle) $ rawSql q qus
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
order by "article_please"."date_publish" desc, "article_please"."id" desc 
limit {lim}
    |]
        []
    person ->
      rawSql
        [qc|
select ??
from "article_please"
inner join named_entity ne on ne.article_please_id = "article_please".id
where ne.entity = ?
  and "article_please".title is not null
group by ("article_please"."id")
order by "article_please"."date_publish" desc, "article_please"."id" desc 
limit {lim}
              |]
        [PersistText person]
  where
    lim = Le.Config.articlesLimit
