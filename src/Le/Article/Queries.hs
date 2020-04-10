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
  rawSql
    [qc|
select ??
from "article_np"
inner join named_entity ne on ne.article_id = "article_np".id
where {personQText}
group by ("article_np"."id")
order by "article_np"."date" desc, "article_np"."id" desc 
limit {lim}
    |]
    [PersistText personQParam]
  where
    lim = Le.Config.articlesLimit
    (personQText, personQParam) =
      case mPerson of
        Nothing -> ("'a'=coalesce('a',?)" :: Text, "a")
        Just person ->
          ("ne.entity = ?", person)
