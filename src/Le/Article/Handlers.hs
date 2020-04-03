module Le.Article.Handlers where

import qualified Database.Persist.Postgresql as P
import qualified Le.ApiTypes as AT
import Le.App
import Le.Article
import Le.Config
import Le.Import
import Le.Model

articlesShortHandler :: Le [AT.ArticleShort]
articlesShortHandler = do
  aNewspapers <- runDb $ P.selectList [ANewspaperDate P.!=. Nothing] [P.Desc ANewspaperDate, P.LimitTo Le.Config.articlesLimit]
  forM aNewspapers $ \article -> do
    let date = fromJustNote "impossible!" (aNewspaperDate (ev article))
    pure $ AT.ArticleShort
      { artDate = zonedTimeToMilliseconds (utcToZonedTime' tz date),
        artPaperName = newspaperNameFromHost (aNewspaperHost (ev article)),
        artTitleShort = aNewspaperTitle (ev article)
      }
