{-# LANGUAGE QuasiQuotes #-}

module Le.Article.Queries where

import qualified Data.Text as T
import Database.Esqueleto
import qualified Le.Config
import Le.Import hiding ((^.), isNothing, on)
import Le.Model
import Text.InterpolatedString.Perl6 (qc)

queryPersonNamedEntities ::
  MonadIO m => Text -> Int -> ReaderT SqlBackend m [Text]
queryPersonNamedEntities query page = do
  let qu = PersistText ("%" <> T.toLower query <> "%")
  fmap (map unSingle) $
    rawSql
      [qc|
SELECT entity FROM "named_entity" ne
WHERE ne.entity ILIKE ?
  AND label_ = 'PERSON'
GROUP BY ne.entity
LIMIT {lim}
OFFSET {(page - 1) * lim}
    |]
      [qu]
  where
    lim = 20

queryPersonArticleNps ::
  MonadIO m => Maybe Text -> ReaderT SqlBackend m [Entity ArticleNp]
queryPersonArticleNps mPerson = do
  case fromMaybe "" (T.strip <$> mPerson) of
    "" ->
      rawSql
        [qc|
select ??
from "article_np"
where "article_np"."date" is not null
order by "article_np"."date" desc, "article_np"."id" desc 
limit {lim}
    |]
        []
    person ->
      rawSql
        [qc|
select ??
from "article_np"
inner join named_entity ne on ne.article_id = "article_np".id
where ne.entity = ?
group by ("article_np"."id")
order by "article_np"."date" desc, "article_np"."id" desc 
limit {lim}
              |]
        [PersistText person]
  where
    lim = Le.Config.articlesLimit
