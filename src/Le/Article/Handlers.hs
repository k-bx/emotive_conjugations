module Le.Article.Handlers where

import qualified Database.Persist.Postgresql as P
import qualified Le.ApiTypes as AT
import Le.App
import Le.AppUtils
import Le.Article
import Le.Config
import Le.Import
import Le.Model

articlesShortHandler :: Le [AT.ArticleShort]
articlesShortHandler = do
  articleNps <- runDb $ P.selectList [ArticleNpDate P.!=. Nothing] [P.Desc ArticleNpDate, P.LimitTo Le.Config.articlesLimit]
  forM articleNps $ \articleNp -> do
    let date = fromJustNote "impossible!" (articleNpDate (ev articleNp))
    pure $ AT.ArticleShort
      { artId = P.toSqlKey (P.fromSqlKey (entityKey articleNp)),
        artDate = zonedTimeToMilliseconds (utcToZonedTime' tz date),
        artPaperName = newspaperNameFromHost (articleNpHost (ev articleNp)),
        artTitleShort = articleNpTitle (ev articleNp)
      }

articleDetails :: ArticleId -> Le AT.Article
articleDetails articleId = do
  articleNp <- mustFindM $ runDb $ P.selectFirst [ArticleNpId P.==. P.toSqlKey (P.fromSqlKey articleId)] []
  pure $ AT.Article
    { arcId = articleId,
      arcUrl = articleNpUrl (ev articleNp),
      arcDate = zonedTimeToMilliseconds . utcToZonedTime' tz <$> articleNpDate (ev articleNp),
      arcPaperName = newspaperNameFromHost (articleNpHost (ev articleNp)),
      arcTitle = articleNpTitle (ev articleNp),
      arcAuthors = unpack (articleNpAuthors (ev articleNp)),
      arcContent = articleNpContent (ev articleNp),
      arcLang = articleNpLang (ev articleNp)
    }
