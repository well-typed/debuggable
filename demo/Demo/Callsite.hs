module Demo.Callsite (Example(..), demo) where

import GHC.Stack (prettyCallStack, callStack)

import Debug.Provenance

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

data Example =
    WithoutDebuggable
  | UseDebuggable
  deriving stock (Show)

demo :: Example -> IO ()
demo WithoutDebuggable = f1
demo UseDebuggable     = g1

{-------------------------------------------------------------------------------
  Without the library
-------------------------------------------------------------------------------}

f1 :: IO ()
f1 = f2

f2 :: HasCallStack => IO ()
f2 = f3

f3 :: HasCallStack => IO ()
f3 = putStrLn $ prettyCallStack callStack

{-------------------------------------------------------------------------------
  Using the library
-------------------------------------------------------------------------------}

g1 :: IO ()
g1 = g2

g2 :: HasCallStack => IO ()
g2 = g3

g3 :: HasCallStack => IO ()
g3 = print callSite
