module Le.Aeson (module Data.Aeson, jsonOpts, jsonOptsReplace) where

import Data.Aeson
import Data.Text hiding (drop)
import Prelude

jsonOpts :: Int -> Options
jsonOpts n =
  defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop n,
      constructorTagModifier = camelTo2 '_' . drop n
    }

jsonOptsReplace :: [(Text, Text)] -> Int -> Options
jsonOptsReplace kvs n =
  let optsBase = jsonOpts n
   in optsBase
        { fieldLabelModifier = \s ->
            let nv = fieldLabelModifier optsBase s
             in case lookup (pack nv) kvs of
                  Nothing -> nv
                  Just v -> unpack v
        }
