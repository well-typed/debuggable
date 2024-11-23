module Demo.Callback (
    withoutDebuggable
  , useDebuggable
  ) where

import GHC.Stack

import Debug.NonInterleavedIO.Scoped qualified as Scoped
import Debug.Provenance.Callback
import Debug.Provenance.Scope

{-------------------------------------------------------------------------------
  Without the library
-------------------------------------------------------------------------------}

f1 :: HasCallStack => (Int -> IO ()) -> IO ()
f1 k = f2 k

f2 :: HasCallStack => (Int -> IO ()) -> IO ()
f2 k = scoped $ k 1

g1 :: HasCallStack => Int -> IO ()
g1 n = g2 n

g2 :: HasCallStack => Int -> IO ()
g2 n = Scoped.putStrLn $ "n = " ++ show n ++ " at " ++ prettyCallStack callStack

withoutDebuggable :: HasCallStack => IO ()
withoutDebuggable = f1 g1

{-------------------------------------------------------------------------------
  Using the library
-------------------------------------------------------------------------------}

h1 :: HasCallStack => Callback IO Int () -> IO ()
h1 k = h2 k

h2 :: HasCallStack => Callback IO Int () -> IO ()
h2 k = scoped $ invokeCallback k 1

useDebuggable :: HasCallStack => IO ()
useDebuggable = h1 (callback g1)

