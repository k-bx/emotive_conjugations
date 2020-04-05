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
  aNewspapers <- runDb $ P.selectList [ANewspaperDate P.!=. Nothing] [P.Desc ANewspaperDate, P.LimitTo Le.Config.articlesLimit]
  forM aNewspapers $ \aNewspaper -> do
    let date = fromJustNote "impossible!" (aNewspaperDate (ev aNewspaper))
    pure $ AT.ArticleShort
      { artId = P.toSqlKey (P.fromSqlKey (entityKey aNewspaper)),
        artDate = zonedTimeToMilliseconds (utcToZonedTime' tz date),
        artPaperName = newspaperNameFromHost (aNewspaperHost (ev aNewspaper)),
        artTitleShort = aNewspaperTitle (ev aNewspaper)
      }

articleDetails :: ArticleId -> Le AT.Article
articleDetails articleId = do
  aNewspaper <- mustFindM $ runDb $ P.selectFirst [ANewspaperId P.==. P.toSqlKey (P.fromSqlKey articleId)] []
  pure $ AT.Article
    { arcId = articleId,
      arcUrl = aNewspaperUrl (ev aNewspaper),
      arcDate = zonedTimeToMilliseconds . utcToZonedTime' tz <$> aNewspaperDate (ev aNewspaper),
      arcPaperName = newspaperNameFromHost (aNewspaperHost (ev aNewspaper)),
      arcTitle = aNewspaperTitle (ev aNewspaper),
      arcAuthors = unpack (aNewspaperAuthors (ev aNewspaper)),
      arcContent = aNewspaperContent (ev aNewspaper),
      arcLang = aNewspaperLang (ev aNewspaper)
    }
