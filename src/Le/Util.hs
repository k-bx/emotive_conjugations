{-# LANGUAGE NoImplicitPrelude #-}

-- | Silly utility module, used to demonstrate how to write a test
-- case.
module Le.Util where

import qualified Data.Set
import Le.Import

eitherErr :: Either String c -> c
eitherErr = either error id

eitherErrShow :: Show a => Either a c -> c
eitherErrShow = either (error . show) id

(|>) :: t1 -> (t1 -> t2) -> t2
(|>) a f = f a

infixl 0 |>

-- | Like `Data.List.nub` but done via 'Data.Set'
nubSet :: Ord a => [a] -> [a]
nubSet xs = Data.Set.toList $ Data.Set.fromList xs
