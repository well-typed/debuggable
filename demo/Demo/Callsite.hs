module Demo.Callsite (
    withoutDebuggable
  , useDebuggable
  ) where

import GHC.Stack (prettyCallStack, callStack)

import Debug.Provenance

{-------------------------------------------------------------------------------
  Without the library
-------------------------------------------------------------------------------}

f1 :: IO ()
f1 = f2

f2 :: HasCallStack => IO ()
f2 = f3

f3 :: HasCallStack => IO ()
f3 = putStrLn $ prettyCallStack callStack

withoutDebuggable :: IO ()
withoutDebuggable = f1

{-------------------------------------------------------------------------------
  Using the library
-------------------------------------------------------------------------------}

g1 :: IO ()
g1 = g2

g2 :: HasCallStack => IO ()
g2 = g3

g3 :: HasCallStack => IO ()
g3 = print callSite

useDebuggable :: IO ()
useDebuggable = g1