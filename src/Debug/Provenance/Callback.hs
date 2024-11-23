{-# LANGUAGE ImplicitParams #-}

-- | Provenance for callbacks
module Debug.Provenance.Callback (
    -- * Callbacks
    Callback -- opaque
  , callback
  , invokeCallback
    -- *** Convenience re-exports
  , HasCallStack
  ) where

import Data.Maybe (fromMaybe)
import GHC.Stack

import Debug.Provenance.Internal

{-------------------------------------------------------------------------------
  Callback
-------------------------------------------------------------------------------}

-- | Callback of type @(a -> m b)@
--
-- When we invoke a callback, it is useful to distinguish between two things:
--
-- * The 'CallStack' of the /invocation/ of the callback
-- * The 'CallSite' of the /definition/ of the callback
--
-- The purpose of this module is to be careful about this distinction; a
-- 'HasCallStack' backtrace originating from an invocation of a callback will
-- look something like this:
--
-- > gM, called at ..
-- > ..
-- > g2, called at ..
-- > g1, called at ..
-- > callbackFn, called at ..
-- > invoking callback defined at <callSite>
-- > invokeCallback, called at ..
-- > fN, called at ..
-- > ..
-- > f2, called at ..
-- > f1, called at ..
--
-- where
--
-- * @f1 .. fN@ are the function calls leading up to the callback
-- * @g1 .. gM@ are the function calls made inside of the callback
-- * @\<callSite\>@ tells us where the callback was defined
newtype Callback m a b = Wrap (Callback_ CallStack m a b)

-- | Define 'Callback'
--
-- See 'Callback' for discussion and motivation of the /two/ 'HasCallStack'
-- constraints.
callback :: HasCallStack => (HasCallStack => a -> m b) -> Callback m a b
callback callbackFn = Wrap (callback_ callSite callbackFn)

-- | Invoke 'Callback'
invokeCallback :: HasCallStack => Callback m a b -> a -> m b
invokeCallback (Wrap cb) a =
    callbackFunction (aux callStack) a
  where
    Callback_{callbackFunction, callbackDefSite} = cb

    aux :: CallStack -> CallStack
    aux = mapCallSites $ \cs ->
        case cs of
          (_, loc):cs' -> -- this is the call to invokeCallback
              ( concat [
                    "invoking callback defined at "
                    -- callee is 'callback', no point showing that
                  , fromMaybe "{unknown}" $
                      callSiteCaller callbackDefSite
                  , maybe "" (\l -> " (" ++ briefSrcLoc l ++ ")") $
                      callSiteSrcLoc callbackDefSite

                  ]
                --      "invoking callback defined at "
                -- ++ prettyCallSite callbackDefSite
              , loc
              )
            : cs'
          [] ->
            error $ "invokeCallback: unexpected CallStack"

{-# NOINLINE callback #-}
{-# NOINLINE invokeCallback #-}

{-------------------------------------------------------------------------------
  Internal: generalize over 'CallStack'

  By working with a polymorphic @cs@ instead of 'CallStack' here, we avoid
  @ghc@ manipulating the 'CallStack' itself. (This of course means that we
  depend on the fact that 'HasCallStack' is defined as an implicit parameter.)
-------------------------------------------------------------------------------}

data Callback_ cs m a b = Callback_ {
      callbackFunction :: !(cs -> a -> m b)
    , callbackDefSite  :: !CallSite
    }

callback_ :: forall cs m a b.
     CallSite
  -> ((?callStack :: cs) => a -> m b)
  -> Callback_ cs m a b
callback_ defSite f = Callback_ (mkExplicit f) defSite

mkExplicit :: ((?callStack :: cs) => a) -> (cs -> a)
mkExplicit f cs = let ?callStack = cs in f

{-# NOINLINE callback_  #-}
{-# NOINLINE mkExplicit #-}

{-------------------------------------------------------------------------------
  Internal: manipulating the callstack
-------------------------------------------------------------------------------}

mapCallSites ::
     ([([Char], SrcLoc)] -> [([Char], SrcLoc)])
  -> CallStack -> CallStack
mapCallSites f = fromCallSiteList . f . getCallStack
