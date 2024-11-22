-- | Utilities for tracking scope: nested 'Invocation's
module Debug.Provenance.Scope (
    -- * Thread-local scope
    Scope
  , scoped
  , getScope
    -- * Scope across threads
  , forkInheritScope
  , inheritScope
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.IORef
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import GHC.Stack
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Map.Strict as Map

import Debug.Provenance

{-------------------------------------------------------------------------------
  Scope
-------------------------------------------------------------------------------}

-- | Thread-local scope
--
-- Most recent invocations are first in the list.
type Scope = [Invocation]

-- | Extend current scope
scoped :: (HasCallStack, MonadMask m, MonadIO m) => CallSite -> m a -> m a
scoped cs k = (\(a, ()) -> a) <$> do
    i <- newInvocation cs
    generalBracket
      (pushInvocation i)
      (\_ _ -> popInvocation)
      (\_ -> k)

-- | Get current scope
getScope :: MonadIO m => m Scope
getScope = modifyThreadLocalScope $ \s -> (s, s)

{-------------------------------------------------------------------------------
  Scope across threads
-------------------------------------------------------------------------------}

-- | Inherit scope from a parent thread
--
-- This sets the scope of the current thread to that of the parent. This should
-- be done prior to growing the scope of the child thread; 'inheritScope' will
-- fail with an exception if the scope in the child thread is not empty.
--
-- See also 'forkInheritScope'.
inheritScope :: MonadIO m => ThreadId -> m ()
inheritScope parent = liftIO $ do
    parentScope <- Map.findWithDefault [] parent <$> readIORef globalScope
    ok          <- modifyThreadLocalScope $ \childScope ->
                     if null childScope
                       then (parentScope, True)
                       else (childScope, False)
    unless ok $ fail "inheritScope: child scope non-empty"

-- | Convenience combination of 'forkIO' and 'inheritScope'
forkInheritScope :: IO () -> IO ThreadId
forkInheritScope child = do
    parent <- myThreadId
    forkIO $ inheritScope parent >> child

{-------------------------------------------------------------------------------
  Internal: scope manipulation
-------------------------------------------------------------------------------}

modifyThreadLocalScope :: forall m a. MonadIO m => (Scope -> (Scope, a)) -> m a
modifyThreadLocalScope f = liftIO $ do
    tid <- myThreadId
    atomicModifyIORef' globalScope $ swap . Map.alterF f' tid
  where
    f' :: Maybe Scope -> (a, Maybe Scope)
    f' = second gcIfEmpty . swap . f . fromMaybe []

    -- Remove the entry from the map altogether if the scope is empty.
    gcIfEmpty :: Scope -> Maybe Scope
    gcIfEmpty [] = Nothing
    gcIfEmpty s  = Just s

modifyThreadLocalScope_ :: MonadIO m => (Scope -> Scope) -> m ()
modifyThreadLocalScope_ f = modifyThreadLocalScope ((,()) . f)

pushInvocation :: MonadIO m => Invocation -> m ()
pushInvocation i = modifyThreadLocalScope_ (i:)

popInvocation :: MonadIO m => m ()
popInvocation = modifyThreadLocalScope_ $ \case
    []  -> error "popInvocation: empty stack"
    _:s -> s

{-------------------------------------------------------------------------------
  Internal: globals
-------------------------------------------------------------------------------}

globalScope :: IORef (Map ThreadId Scope)
{-# NOINLINE globalScope #-}
globalScope = unsafePerformIO $ newIORef Map.empty

