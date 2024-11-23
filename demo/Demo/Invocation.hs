module Demo.Invocation (
    Example(..)
  , withoutDebuggable
  , useDebuggable
  ) where

import Control.Monad

import Debug.Provenance

data Example =
    Example1
  | Example2
  deriving stock (Show)

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

withoutDebuggable :: IO ()
withoutDebuggable = f4

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

useDebuggable :: Example -> IO ()
useDebuggable Example1 = g1
useDebuggable Example2 = g4