module Demo.Invocation (
    Example(..)
  , DebuggableExample(..)
  , demo
  ) where

import Control.Monad

import Debug.Provenance

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

data Example =
    WithoutDebuggable
  | UseDebuggable DebuggableExample
  deriving stock (Show)

data DebuggableExample =
    Example1
  | Example2
  deriving stock (Show)

demo :: Example -> IO ()
demo WithoutDebuggable        = f4
demo (UseDebuggable Example1) = g1
demo (UseDebuggable Example2) = g4

{-------------------------------------------------------------------------------
  Without the library
-------------------------------------------------------------------------------}

f4 :: IO ()
f4 = do
    putStrLn "f4:1"
    -- f4 does something ..
    putStrLn "f4:2"
    -- f4 does something else ..
    putStrLn "f4:3"

{-------------------------------------------------------------------------------
  Using the library
-------------------------------------------------------------------------------}

g1 :: IO ()
g1 = replicateM_ 2 g2

g2 :: HasCallStack => IO ()
g2 = do
    print =<< newInvocation
    replicateM_ 2 g3

g3 :: HasCallStack => IO ()
g3 = print =<< newInvocation

g4 :: HasCallStack => IO ()
g4 = do
    print =<< newInvocation
    -- f4 does something ..
    print =<< newInvocation
    -- f4 does something else ..
    print =<< newInvocation
