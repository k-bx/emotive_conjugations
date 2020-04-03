module Le.Shake where

import Development.Shake
import Le.Import

shake :: IO ()
shake = do
  shakeArgs shakeOptions {shakeFiles = "static/css"} $ do
    want ["static/css/index.css"]
    "static/css//*.css" %> \_out -> do
      scsss <- getDirectoryFiles "" ["static/css//*.scss"]
      need scsss
      cmd_
        ["sass" :: String]
        ("--source-map" :: String)
        ("--error-css" :: String)
        ["static/css/index.scss" :: String]
        ["static/css/index.css" :: String]
