{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Spec.Infra.Resources where

import Hipsql.Internal (PsqlIO(..))

data TestResources = TestResources
  { readStdout' :: IO String
  , writeStdin' :: String -> IO ()
  , psqlIO' :: PsqlIO
  }

testResources :: (HasTestResources) => TestResources
testResources@TestResources
  { readStdout' = readStdout
  , writeStdin' = writeStdin
  , psqlIO' = psqlIO
  } = ?resources

type HasTestResources = (?resources :: TestResources)
