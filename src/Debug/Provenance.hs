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
  ) where

import Control.Monad.IO.Class
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.IORef
import Data.List (intercalate)
import GHC.Generics
import GHC.Stack
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.HashMap.Strict as HashMap

{-------------------------------------------------------------------------------
  Callsites
-------------------------------------------------------------------------------}

-- | Callsite
--
-- A callsite tells you where something was called: a location in the source,
-- and the name of the function that did the calling. Optionally, they can be
-- given an additional user-defined label also.
--
-- /NOTE/: If you are seeing @<unknown>@ instead of the function name,
-- the calling function does not have a 'HasCallStack' annotation:
--
-- > yourFunction :: HasCallStack => IO () -- 'HasCallStack' probably missing
-- > yourFunction = do
-- >     let cs = callSite
-- >     ..
--
-- Once you add this annotation, you should see @yourFunction@ instead of
-- @<unknown>@. Similarly, if you have local function definitions, it may
-- be useful to give those 'HasCallStack' constraints of their own:
--
-- > yourFunction :: HasCallStack => IO ()
-- > yourFunction = ..
-- >   where
-- >     someLocalFn :: HasCallStack => IO ()
-- >     someLocalFn = do
-- >         let cs = callSite
-- >         ..
--
-- In this example the 'HasCallStack' constraint on @someLocalFn@ means that the
-- calling function will be reported as @someLocalFn@ instead of @yourFunction@.
data CallSite = CallSite {
      callSiteSrcLoc   :: SrcLoc
    , callSiteFunction :: Maybe String
    , callSiteLabel    :: Label
    }
  deriving stock (Eq)

-- | Label associated with 'CallSite'
--
-- This is an internal type.
data Label = Label String | NoLabel
  deriving stock (Eq, Generic)
  deriving anyclass (Hashable)

-- | Render 'CallSite' to human-readable format
prettyCallSite :: CallSite -> String
prettyCallSite CallSite{callSiteSrcLoc, callSiteFunction, callSiteLabel} =
    concat [
        case callSiteFunction of
           Nothing -> "<unknown>"
           Just fn -> fn
      , "("
      , intercalate ":" [
            srcLocFile callSiteSrcLoc
          , show $ srcLocStartLine callSiteSrcLoc
          , show $ srcLocStartCol callSiteSrcLoc
          ]
      , case callSiteLabel of
          NoLabel -> ""
          Label l -> "," ++ show l
      , ")"
      ]

instance Hashable CallSite where
  hashWithSalt salt CallSite{callSiteSrcLoc, callSiteFunction, callSiteLabel} =
      hashWithSalt salt (
          prettySrcLoc callSiteSrcLoc
        , callSiteFunction
        , callSiteLabel
        )

-- | Current 'CallSite'
callSite :: HasCallStack => CallSite
callSite = withFrozenCallStack $ mkCallSite NoLabel

-- | Current 'CallSite' with user-defined label
callSiteWithLabel :: String -> CallSite
callSiteWithLabel label = withFrozenCallStack $ mkCallSite (Label label)

-- | Internal auxiliary to 'callSite' and 'callSiteWithLabel'
mkCallSite  :: HasCallStack => Label -> CallSite
mkCallSite callSiteLabel = aux callStack
  where
    aux :: CallStack -> CallSite
    aux cs =
        case getCallStack cs of
          (_, loc) : [] -> CallSite {
              callSiteSrcLoc   = loc
            , callSiteFunction = Nothing
            , callSiteLabel
            }
          (_, loc) : (fn, _) : _ -> CallSite {
              callSiteSrcLoc   = loc
            , callSiteFunction = Just fn
            , callSiteLabel
            }
          [] ->
            bug "callSite: emptycallstack"

{-------------------------------------------------------------------------------
  Invocations
-------------------------------------------------------------------------------}

-- | Invocation
--
-- An invocation not only tells you the /where/, but also the /when/: it pairs a
-- 'CallSite' with a count, automatically incremented on each call to
-- 'newInvocation'. Each 'CallSite' uses its own counter.
data Invocation = Invocation CallSite Int
  deriving stock (Eq)

-- | Render 'Invocation' to human-readable format
prettyInvocation :: Invocation -> String
prettyInvocation (Invocation cs n) =
    concat [
        "\""
      , prettyCallSite cs
      , "/" ++ show n
      , "\""
      ]

-- | New invocation
--
-- See 'Invocation' for discussion.
newInvocation :: (MonadIO m, HasCallStack) => CallSite -> m Invocation
newInvocation cs = liftIO $ do
    atomicModifyIORef' globalCounters $ \counters ->
      let i = HashMap.findWithDefault 1 cs counters
      in (HashMap.insert cs (succ i) counters, Invocation cs i)

{-------------------------------------------------------------------------------
  Internal: auxiliary
-------------------------------------------------------------------------------}

bug :: String -> a
bug str = error . unlines $ [
      str
    , "Please report this as a bug at https://github.com/well-typed/niio"
    ]

{-------------------------------------------------------------------------------
  Internal: globals
-------------------------------------------------------------------------------}

globalCounters :: IORef (HashMap CallSite Int)
{-# NOINLINE globalCounters #-}
globalCounters = unsafePerformIO $ newIORef HashMap.empty
