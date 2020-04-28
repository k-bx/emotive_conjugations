{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Either.ValidationT where

import qualified Data.Either.Combinators as Either
import Data.Either.Validation
import Data.Functor.Compose
import RIO

newtype ValidateT e m a =
  ValidateT (Compose m (Validation e) a)
  deriving (Functor, Applicative)

runValidateT :: ValidateT e f a -> f (Validation e a)
runValidateT (ValidateT c) = getCompose c

bimapValidateT ::
     Functor m
  => (b0 -> b1)
  -> (a0 -> a1)
  -> ValidateT b0 m a0
  -> ValidateT b1 m a1
bimapValidateT f1 f2 (ValidateT (Compose c)) =
  ValidateT $
  Compose $
  fmap (eitherToValidation . Either.mapBoth f1 f2 . validationToEither) c

throwVT :: Applicative m => err -> ValidateT err m a
throwVT err = ValidateT (Compose (pure (Failure err)))
