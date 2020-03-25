{-# LANGUAGE NoImplicitPrelude #-}

-- | Silly utility module, used to demonstrate how to write a test
-- case.
module Le.Util where

import Le.Import

eitherErr :: Either String c -> c
eitherErr = either error id
