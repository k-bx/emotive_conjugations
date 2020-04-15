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
  articlesPlease <- runDb $ Q.queryPersonArticlesPlease mPerson
  forM articlesPlease $ \articlePlease -> do
    let mdate = articlePleaseDatePublish (ev articlePlease)
    pure $ AT.ArticleShort
      { artId = P.toSqlKey (P.fromSqlKey (entityKey articlePlease)),
        artDate = zonedTimeToMilliseconds . utcToZonedTime' tz <$> mdate,
        artPaperName = newspaperNameFromHost (articlePleaseHost (ev articlePlease)),
        artTitleShort = fromMaybe "<unknown>" (articlePleaseTitle (ev articlePlease))
      }

articleDetails :: ArticleId -> Le AT.Article
articleDetails articleId = do
  let articlePleaseId = P.toSqlKey (P.fromSqlKey articleId)
  articlePlease <- mustFindM $ runDb $ P.get articlePleaseId
  pure $ AT.Article
    { arcId = articleId,
      arcUrl = articlePleaseUrl articlePlease,
      arcDate = zonedTimeToMilliseconds . utcToZonedTime' tz <$> articlePleaseDatePublish articlePlease,
      arcPaperName = newspaperNameFromHost (articlePleaseHost articlePlease),
      arcTitle = fromMaybe "<unknown>" (articlePleaseTitle articlePlease),
      arcAuthors = unpack (articlePleaseAuthors articlePlease),
      arcLang = articlePleaseLanguage articlePlease
    }

articlePleaseDetails :: ArticlePleaseId -> Le AT.ArticlePlease
articlePleaseDetails articleNpId = do
  articlePlease <- mustFindM $ runDb $ P.get articleNpId
  pure $ AT.ArticlePlease
    { arpId = articleNpId,
      arpAuthors = unpack (articlePleaseAuthors articlePlease),
      arpDatePublish = zonedTimeToMilliseconds . utcToZonedTime' tz <$> articlePleaseDatePublish articlePlease,
      arpMaintext = articlePleaseMaintext articlePlease,
      arpLanguage = Just (articlePleaseLanguage articlePlease),
      arpSpacyNerEnts = fmap Le.Python.csrEnts (articlePleaseSpacyNer articlePlease),
      arpSpacyPosEnts = fmap Le.Python.cprTokens (articlePleaseSpacyPos articlePlease)
    }

listNamedEntities :: Maybe Text -> Maybe Int -> Le (AT.Paginated Text)
listNamedEntities mQuery mPage = do
  let page = fromMaybe 1 mPage
      query = fromMaybe "" mQuery
  entities <- runDb $ Q.queryPersonNamedEntities query page
  pure $ AT.Paginated
    { pgnItems = entities,
      pgnOverallPages =
        if length entities >= Le.Config.entitiesPerPage
          then page + 1
          else page
    }
