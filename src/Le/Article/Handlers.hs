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
import Le.Time
import Le.Util

articlesShortHandler ::
  Maybe Text ->
  Le [AT.ArticleShort]
articlesShortHandler mPerson = sg $ do
  articlesPlease <- runDb $ Q.queryPersonArticlesPlease mPerson
  forM articlesPlease $ \articlePlease -> do
    let mdate = articlePleaseDatePublish (ev articlePlease)
    pure $
      AT.ArticleShort
        { artId = P.toSqlKey (P.fromSqlKey (entityKey articlePlease)),
          artDate = renderTime <$> mdate,
          artPaperName = newspaperNameFromHost (articlePleaseHost (ev articlePlease)),
          artTitleShort = fromMaybe "<unknown>" (articlePleaseTitle (ev articlePlease))
        }

articleDetails :: ArticleId -> Le AT.Article
articleDetails articleId = sg $ do
  let articlePleaseId = P.toSqlKey (P.fromSqlKey articleId)
  article <- mustFindM $ runDb $ P.get articleId
  articlePlease <- mustFindM $ runDb $ P.get articlePleaseId
  pure $
    AT.Article
      { arcId = articleId,
        arcUrl = articlePleaseUrl articlePlease,
        arcDate = renderTime <$> articlePleaseDatePublish articlePlease,
        arcPaperName = newspaperNameFromHost (articlePleaseHost articlePlease),
        arcTitle = fromMaybe "<unknown>" (articlePleaseTitle articlePlease),
        arcAuthors = unpack (articlePleaseAuthors articlePlease),
        arcLang = articlePleaseLanguage articlePlease,
        arcWarcId = articleWarcId article
      }

articlePleaseDetails :: ArticlePleaseId -> Le AT.ArticlePlease
articlePleaseDetails articleNpId = sg $ do
  articlePlease <- mustFindM $ runDb $ P.get articleNpId
  pure $
    AT.ArticlePlease
      { arpId = articleNpId,
        arpAuthors = unpack (articlePleaseAuthors articlePlease),
        arpDateDownload = renderTime <$> articlePleaseDateDownload articlePlease,
        arpDatePublish = renderTime <$> articlePleaseDatePublish articlePlease,
        arpDateModify = renderTime <$> articlePleaseDateModify articlePlease,
        -- arpMaintext = articlePleaseMaintext articlePlease,
        arpLanguage = Just (articlePleaseLanguage articlePlease)
        -- arpSpacyNerEnts = fmap Le.Python.csrEnts (articlePleaseSpacyNer articlePlease),
        -- arpSpacyPosEnts = fmap Le.Python.cprTokens (articlePleaseSpacyPos articlePlease)
      }

articlePleaseDetailsBig :: ArticlePleaseBigId -> Le AT.ArticlePleaseBig
articlePleaseDetailsBig bigId = sg $ do
  articlePleaseBig <- mustFindM $ runDb $ P.get bigId
  pure $
    AT.ArticlePleaseBig
      { arbId = bigId,
        arbMaintext = articlePleaseBigMaintext articlePleaseBig,
        arbSpacyNerEnts = fmap Le.Python.csrEnts (articlePleaseBigSpacyNer articlePleaseBig),
        arbSpacyPosEnts = fmap Le.Python.cprTokens (articlePleaseBigSpacyPos articlePleaseBig)
      }

listNamedEntities :: Maybe Text -> Maybe Int -> Le (AT.Paginated Text)
listNamedEntities mQuery mPage = sg $ do
  let page = fromMaybe 1 mPage
      query = fromMaybe "" mQuery
  entities <- runDb $ Q.queryPersonNamedEntities query page
  pure $
    AT.Paginated
      { pgnItems = entities,
        pgnOverallPages =
          if length entities >= Le.Config.entitiesPerPage
            then page + 1
            else page
      }

namedEntityGroup ::
  Maybe Text ->
  Le AT.NamedEntityGroup
namedEntityGroup mNer = sg $ do
  let ner = fromMaybe "" mNer
  mNamedProper <- runDb $ P.get (NamedPropersKey ner)
  case mNamedProper of
    Nothing ->
      pure $
        AT.NamedEntityGroup
          { nerEntity = ner,
            nerGroup = []
          }
    Just namedProper -> do
      let proper = namedPropersProper namedProper
      propers <- runDb $ P.selectList [NamedPropersProper P.==. proper] []
      pure $
        AT.NamedEntityGroup
          { nerEntity = ner,
            nerGroup =
              propers
                |> map (namedPropersEntity . ev)
          }
