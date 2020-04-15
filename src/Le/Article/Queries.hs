{-# LANGUAGE QuasiQuotes #-}

module Le.Article.Queries where

import qualified Data.Text as T
import Database.Esqueleto
import qualified Le.Config
import Le.Import hiding ((^.), isNothing, on)
import Le.Model
import Text.InterpolatedString.Perl6 (qc)
import qualified Prelude

queryPersonNamedEntities ::
  MonadIO m => Text -> Int -> ReaderT SqlBackend m [Text]
queryPersonNamedEntities query page = do
  fmap concat $ forM (T.words query) $ \word -> do
    let qu = PersistText (T.toLower word <> "%")
    let q =
          [qc|
    SELECT entity FROM "named_entity" ne
    WHERE label_ = 'PERSON'
      AND (ne.search1 like ? or ne.search2 like ? or ne.search3 like ?)
    GROUP BY ne.entity
    LIMIT {lim}
    OFFSET {(page - 1) * lim}
        |]
    liftIO $ Prelude.putStrLn (show (q, [qu, qu, qu]))
    fmap (map unSingle) $ rawSql q [qu, qu, qu]
  where
    lim = 20

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
