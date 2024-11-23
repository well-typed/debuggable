module Debug.Provenance.Internal (
    -- * Callsites
    CallSite(..)
  , prettyCallSite
  , briefSrcLoc
  , callSite
  , callSiteWithLabel
    -- * Invocations
  , Invocation -- opaque
  , prettyInvocation
  , newInvocation
  , newInvocationFrom
  ) where

import Control.Monad.IO.Class
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.IORef
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import GHC.Generics
import GHC.Stack
import System.IO.Unsafe (unsafePerformIO)

{-------------------------------------------------------------------------------
  Callsites
-------------------------------------------------------------------------------}

-- | Callsite
--
-- A callsite tells you where something was called: a location in the source,
-- and the name of the function that did the calling. Optionally, they can be
-- given an additional user-defined label also.
--
-- /NOTE/: If you are seeing @{unknown}@ instead of the function name,
-- the calling function does not have a 'HasCallStack' annotation:
--
-- > yourFunction :: HasCallStack => IO () -- 'HasCallStack' probably missing
-- > yourFunction = do
-- >     let cs = callSite
-- >     ..
--
-- Once you add this annotation, you should see @yourFunction@ instead of
-- @{unknown}@. Similarly, if you have local function definitions, it may
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
      callSiteSrcLoc :: Maybe SrcLoc
    , callSiteCaller :: Maybe String
    , callSiteCallee :: Maybe String
    , callSiteLabel  :: Label
    }
  deriving stock (Eq)

instance Show CallSite where
  show = prettyCallSite

-- | Label associated with 'CallSite'
--
-- This is an internal type.
data Label = Label String | NoLabel
  deriving stock (Eq, Generic)
  deriving anyclass (Hashable)

-- | Render 'CallSite' to human-readable format
prettyCallSite :: CallSite -> String
prettyCallSite cs =
    concat [
        intercalate " -> " [
            fromMaybe "{unknown}" callSiteCaller
          , fromMaybe "{unknown}" callSiteCallee
          ]
      , " ("
      , intercalate ", " $ concat [
            [ briefSrcLoc loc
            | Just loc <- [callSiteSrcLoc]
            ]
          , [ show label
            | Label label <- [callSiteLabel]
            ]
          ]
      , ")"
      ]
  where
    CallSite{
        callSiteSrcLoc
      , callSiteCaller
      , callSiteCallee
      , callSiteLabel
      } = cs

-- | Variant on 'prettySrcLoc' which omits the package and module name
briefSrcLoc :: SrcLoc -> [Char]
briefSrcLoc loc = intercalate ":" [
      srcLocFile loc
    , show $ srcLocStartLine loc
    , show $ srcLocStartCol loc
    ]

instance Hashable CallSite where
  hashWithSalt salt cs =
      hashWithSalt salt (
          prettySrcLoc <$> callSiteSrcLoc
        , callSiteCaller
        , callSiteCallee
        , callSiteLabel
        )
    where
      CallSite{
          callSiteSrcLoc
        , callSiteCaller
        , callSiteCallee
        , callSiteLabel
        } = cs


-- | Current 'CallSite'
callSite :: HasCallStack => CallSite
callSite = withFrozenCallStack $ mkCallSite NoLabel

-- | Current 'CallSite' with user-defined label
callSiteWithLabel :: HasCallStack => String -> CallSite
callSiteWithLabel label = withFrozenCallStack $ mkCallSite (Label label)

-- | Internal auxiliary to 'callSite' and 'callSiteWithLabel'
mkCallSite  :: HasCallStack => Label -> CallSite
mkCallSite callSiteLabel = aux callStack
  where
    aux :: CallStack -> CallSite
    aux cs =
        -- drop the call to @callSite{withLabel}@
        case getCallStack cs of
          _ : (callee, loc) : [] -> CallSite {
              callSiteSrcLoc = Just loc
            , callSiteCaller = Nothing
            , callSiteCallee = Just callee
            , callSiteLabel
            }
          _ : (callee, loc) : (caller, _) : _ -> CallSite {
              callSiteSrcLoc   = Just loc
            , callSiteCaller = Just caller
            , callSiteCallee = Just callee
            , callSiteLabel
            }
          _otherwise -> CallSite {
              callSiteSrcLoc = Nothing
            , callSiteCaller = Nothing
            , callSiteCallee = Nothing
            , callSiteLabel
            }

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

instance Show Invocation where
  show = prettyInvocation

-- | Render 'Invocation' to human-readable format
prettyInvocation :: Invocation -> String
prettyInvocation (Invocation cs n) =
    concat [
        fromMaybe "{unknown}" callSiteCaller
      , " ("
      , intercalate ", " $ concat [
            [ intercalate ":" [
                  srcLocFile loc
                , show $ srcLocStartLine loc
                , show $ srcLocStartCol loc
                ]
            | Just loc <- [callSiteSrcLoc]
            ]
          , [ show label
            | Label label <- [callSiteLabel]
            ]
          ]
      , ") #"
      , show n
      ]
  where
    -- the callee is 'newInvocation'
    CallSite{
        callSiteSrcLoc
      , callSiteCaller
      , callSiteLabel
      } = cs

-- | New invocation
--
-- See 'Invocation' for discussion.
newInvocation :: (HasCallStack, MonadIO m) => m Invocation
newInvocation =
    -- We intentionally do /NOT/ freeze the callstack here: when function @foo@
    -- calls @newInvocation@, we want a 'CallSite' of @foo -> newInvocation@,
    -- not @bar -> foo@.
    newInvocationFrom callSite

-- | Generalization of 'newInvocation'
newInvocationFrom :: MonadIO m => CallSite -> m Invocation
newInvocationFrom cs = liftIO $ do
    atomicModifyIORef' globalCounters $ \counters ->
      let i = HashMap.findWithDefault 1 cs counters
      in (HashMap.insert cs (succ i) counters, Invocation cs i)

{-------------------------------------------------------------------------------
  Internal: globals
-------------------------------------------------------------------------------}

globalCounters :: IORef (HashMap CallSite Int)
{-# NOINLINE globalCounters #-}
globalCounters = unsafePerformIO $ newIORef HashMap.empty
