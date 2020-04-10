module Le.Article.Handlers where

import qualified Database.Persist.Postgresql as P
import qualified Le.ApiTypes as AT
import Le.App
import Le.AppUtils
import Le.Article
import qualified Le.Article.Queries as Q
import Le.Config
import Le.Import
import Le.Model
import qualified Le.Python

articlesShortHandler ::
  Maybe Text ->
  Le [AT.ArticleShort]
articlesShortHandler mPerson = do
  articleNps <- runDb $ Q.queryPersonArticleNps mPerson
  forM articleNps $ \articleNp -> do
    let mdate = articleNpDate (ev articleNp)
    pure $ AT.ArticleShort
      { artId = P.toSqlKey (P.fromSqlKey (entityKey articleNp)),
        artDate = zonedTimeToMilliseconds . utcToZonedTime' tz <$> mdate,
        artPaperName = newspaperNameFromHost (articleNpHost (ev articleNp)),
        artTitleShort = articleNpTitle (ev articleNp)
      }

articleDetails :: ArticleId -> Le AT.Article
articleDetails articleId = do
  let articleNpId = P.toSqlKey (P.fromSqlKey articleId)
  articleNp <- mustFindM $ runDb $ P.get articleNpId
  pure $ AT.Article
    { arcId = articleId,
      arcUrl = articleNpUrl articleNp,
      arcDate = zonedTimeToMilliseconds . utcToZonedTime' tz <$> articleNpDate articleNp,
      arcPaperName = newspaperNameFromHost (articleNpHost articleNp),
      arcTitle = articleNpTitle articleNp,
      arcAuthors = unpack (articleNpAuthors articleNp),
      arcLang = articleNpLang articleNp
    }

articleNpDetails :: ArticleNpId -> Le AT.ArticleNp
articleNpDetails articleNpId = do
  articleNp <- mustFindM $ runDb $ P.get articleNpId
  pure $ AT.ArticleNp
    { arnId = articleNpId,
      arnAuthors = unpack (articleNpAuthors articleNp),
      arnDate = zonedTimeToMilliseconds . utcToZonedTime' tz <$> articleNpDate articleNp,
      arnContent = articleNpContent articleNp,
      arnLang = articleNpLang articleNp,
      arnSpacyNerEnts = fmap Le.Python.csrEnts (articleNpSpacyNer articleNp),
      arnSpacyPosEnts = fmap Le.Python.cprTokens (articleNpSpacyPos articleNp)
    }

listNamedEntities :: Maybe Text -> Maybe Int -> Le (AT.Paginated Text)
listNamedEntities mQuery mPage = do
  let page = fromMaybe 1 mPage
      query = fromMaybe "" mQuery
  entities <- runDb $ Q.queryPersonNamedEntities query page
  pure $ AT.Paginated
    { pgnItems = entities,
      pgnOverallPages = page + 1
    }
