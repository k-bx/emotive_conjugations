{-# LANGUAGE QuasiQuotes #-}

module Le.Article.Queries where

import qualified Data.Text as T
import Database.Esqueleto
import Le.Import hiding ((^.), isNothing, on)
import Text.InterpolatedString.Perl6 (qc)

queryNamedEntities ::
  MonadIO m => Text -> Int -> ReaderT SqlBackend m [Text]
queryNamedEntities query page = do
  let qu = PersistText ("%" <> T.toLower query <> "%")
  fmap (map unSingle) $
    rawSql
      [qc|
SELECT entity FROM "named_entity" ne
WHERE ne.entity ILIKE ?
GROUP BY ne.entity
LIMIT {lim}
OFFSET {(page - 1) * lim}
    |]
      [qu]
  where
    lim = 20
