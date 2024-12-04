module Demo.NIIO (Example(..), demo) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import System.IO

import Debug.NonInterleavedIO qualified as NIIO

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

data Example =
    WithoutDebuggable
  | UseDebuggable
  deriving stock (Show)

demo :: Example -> IO ()
demo WithoutDebuggable = withoutDebuggable
demo UseDebuggable     = useDebuggable

{-------------------------------------------------------------------------------
  Without the library
-------------------------------------------------------------------------------}

withoutDebuggable :: IO ()
withoutDebuggable = do
    hSetBuffering stdout NoBuffering

    concurrently_
      ( replicateM_ 10 $ do
          putStrLn "This is a message from the first thread"
          threadDelay 100_000
      )
      ( replicateM_ 10 $ do
          putStrLn "And this is a message from the second thread"
          threadDelay 100_000
      )

{-------------------------------------------------------------------------------
  Using the library
-------------------------------------------------------------------------------}

useDebuggable :: IO ()
useDebuggable = do
    concurrently_
      ( replicateM_ 10 $ do
          NIIO.putStrLn "This is a message from the first thread"
          threadDelay 100_000
      )
      ( replicateM_ 10 $ do
          NIIO.putStrLn "And this is a message from the second thread"
          threadDelay 100_000
      )
