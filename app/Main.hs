{-# LANGUAGE TemplateHaskell #-}

module Main
  ( main,
  )
where

import Le.Import
import qualified Le.Run
import Options.Applicative.Simple
import qualified Paths_emotive_conjugations

main :: IO ()
main =
  Le.Run.main
    $(simpleVersion Paths_emotive_conjugations.version)
