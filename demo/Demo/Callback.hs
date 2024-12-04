module Demo.Callback (Example(..), demo) where

import GHC.Stack

import Debug.NonInterleavedIO.Scoped qualified as Scoped
import Debug.Provenance.Callback
import Debug.Provenance.Scope

import Demo.Callback.Profiling qualified as Profiling

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

data Example =
    WithoutDebuggable
  | UseDebuggable
  | UseProfiling (Maybe Int)
  deriving stock (Show)

demo :: Example -> IO ()
demo WithoutDebuggable = f1 g1
demo UseDebuggable     = h1 (callback g1)
demo (UseProfiling i)  = Profiling.demo i

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

{-------------------------------------------------------------------------------
  Using the library
-------------------------------------------------------------------------------}

h1 :: HasCallStack => Callback IO Int () -> IO ()
h1 k = h2 k

h2 :: HasCallStack => Callback IO Int () -> IO ()
h2 k = scoped $ invokeCallback k 1
