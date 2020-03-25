{-# LANGUAGE NoImplicitPrelude #-}

module UtilSpec
  ( spec,
  )
where

import Le.Import
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "plus2" $ do
    it "basic check" $ ((2 :: Int) + 0) `shouldBe` 2
    it "overflow" $ ((2 :: Int) + maxBound) `shouldBe` minBound + 1
    prop "minus 2" $ \i -> ((2 :: Int) + i) - 2 `shouldBe` i
