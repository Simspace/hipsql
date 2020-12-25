{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Spec.Infra
  ( module Spec.Infra
  , module Spec.Infra.Resources
  ) where

import Control.Concurrent.Async (race)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.ByteString (ByteString)
import Hipsql.Internal (PsqlIO(..), renderTable, renderXTable, startPsqlWith', withLibPQConnect)
import Spec.Infra.Resources
import System.Timeout (timeout)
import Test.Hspec (Spec, expectationFailure, it)
import qualified Data.ByteString.Char8 as Char8

-- | Shorthand for a rendered table.
table :: [ByteString] -> [[ByteString]] -> String
table hs rs = Char8.unpack $ renderTable (Just hs) rs <> "\n"

-- | Shorthand for a rendered "extended display" table.
xtable :: [ByteString] -> [[ByteString]] -> String
xtable hs rs = Char8.unpack $ renderXTable hs rs <> "\n"

-- | A single test case using 'withTestResources'.
test' :: String -> ((HasTestResources) => IO ()) -> Spec
test' name f =
  it name
    $ withTimeout' 5 name
    $ withTestResources \r -> let ?resources = r in f

-- | A single test case using 'withTestResources'. Spawns
-- a pseudo psql session via 'race' and asserts that the
-- test completes before the psql session does.
test :: String -> ((HasTestResources) => IO ()) -> Spec
test name f = do
  test' name do
    race (startPsqlWith' psqlIO withLibPQConnect) f >>= \case
      Right () -> pure ()
      Left () ->
        expectationFailure
          "psql session unexpectedly terminated before test completed"

withTestResources :: (TestResources -> IO ()) -> IO ()
withTestResources body = do
  let mkStream name = do
        chan <- newChan
        let w s = withTimeout ("write" <> name <> " " <> show s) (writeChan chan s)
        let r = withTimeout ("read" <> name) (readChan chan)
        pure (w, r)
  (writeStdout, readStdout') <- mkStream "Stdout"
  (writeStdin', readStdin) <- mkStream "Stdin"
  body TestResources
    { readStdout'
    , writeStdin'
    , psqlIO' = PsqlIO
        { inputStrLn' = \s -> liftIO $ writeStdout s >> Just <$> readStdin
        , writeStrLn' = liftIO . writeStdout
        , writeBSLn' = liftIO . writeStdout . Char8.unpack
        }
    }

withTimeout :: String -> IO a -> IO a
withTimeout = withTimeout' 2

withTimeout' :: Int -> String -> IO a -> IO a
withTimeout' seconds name action =
  timeout (seconds * 1000000) action >>= \case
    Just a -> pure a
    Nothing -> do
      expectationFailure
        ("Failed to complete " <> show name
          <> " within " <> show seconds <> " seconds")
      error "can't get here"
