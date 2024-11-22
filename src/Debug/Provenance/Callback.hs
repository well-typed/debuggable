{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -ddump-simpl -dsuppress-all -ddump-to-file #-}

-- | Provenance for callbacks
module Debug.Provenance.Callback (
    Callback -- opaque
  , callback
  , invokeCallback
  ) where

import GHC.Stack

import Debug.Provenance

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
callback callbackFn = withFrozenCallStack $ Wrap (callback_ callbackFn)

-- | Invoke 'Callback'
invokeCallback :: HasCallStack => Callback m a b -> a -> m b
invokeCallback (Wrap cb) a = invoke_ aux cb a
  where
    aux :: CallSite -> CallStack -> CallStack
    aux defSite = mapCallSites $ \cs ->
        case cs of
          (_, loc):cs' -> -- this is the call to invoke_
              ( "invoking callback defined at " ++ prettyCallSite defSite
              , loc
              )
            : cs'
          _otherwise ->
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
     HasCallStack
  => ((?callStack :: cs) => a -> m b)
  -> Callback_ cs m a b
callback_ f = Callback_ (mkExplicit f) callSite

invoke_ ::
     (?callStack :: cs)
  => (CallSite -> cs -> cs)
  -> Callback_ cs m a b -> a -> m b
invoke_ g Callback_{callbackFunction = fn, callbackDefSite = defSite} a =
    mkImplicit (\cs -> fn (g defSite cs) a)

mkExplicit :: ((?callStack :: cs) => a) -> (cs -> a)
mkExplicit f cs = let ?callStack = cs in f

mkImplicit :: (?callStack :: cs) => (cs -> a) -> a
mkImplicit f = f ?callStack

{-# NOINLINE callback_  #-}
{-# NOINLINE invoke_    #-}
{-# NOINLINE mkExplicit #-}
{-# NOINLINE mkImplicit #-}

{-------------------------------------------------------------------------------
  Internal: manipulating the callstack
-------------------------------------------------------------------------------}

mapCallSites ::
     ([([Char], SrcLoc)] -> [([Char], SrcLoc)])
  -> CallStack -> CallStack
mapCallSites f = fromCallSiteList . f . getCallStack
