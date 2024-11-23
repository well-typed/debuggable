-- | Utilities for tracking provenance: where and when things are called
module Debug.Provenance (
    -- * Callsites
    CallSite -- opaque
  , prettyCallSite
  , callSite
  , callSiteWithLabel
    -- * Invocations
  , Invocation -- opaque
  , prettyInvocation
  , newInvocation
    -- *** Convenience re-exports
  , HasCallStack
  ) where

import GHC.Stack

import Debug.Provenance.Internal
