{-# OPTIONS_GHC -Wno-orphans #-}

-- | Functions for non-interleaved output
--
-- Intended for unqualified import.
module System.IO.NonInterleaved (
    -- * Output functions
    niPutStr
  , niPutStrLn
  , niPrint
    -- * Tracing functions
  , niTrace
  , niTraceShow
  , niTraceShowId
  , niTraceM
  , niTraceShowM
    -- * Uniques
  , NiUnique -- opaque
  , niGetUnique
  , niPutStrAt
    -- * Derived functionality
  , niBracket
  ) where

import Control.Concurrent
import Control.Monad.Catch (MonadMask, ExitCase(..), generalBracket)
import Control.Monad.IO.Class
import Data.Hashable (Hashable(..))
import Data.HashMap.Strict (HashMap)
import Data.IORef
import Data.List (intercalate)
import GHC.Stack
import System.Environment
import System.IO
import System.IO.Temp (getCanonicalTemporaryDirectory)
import System.IO.Unsafe

import qualified Data.HashMap.Strict as HashMap

{-------------------------------------------------------------------------------
  Output functions
-------------------------------------------------------------------------------}

-- | Non-interleaved version of 'putStr'
--
-- Concurrent calls to 'niPutStr' will not result in interleaved output.
--
-- The first output will create a new temporary file (typically in @/tmp@,
-- depending on the OS), and a message is written to 'stderr' with the name of
-- the file. If you prefer to specify which file to write to, you can set the
-- @NIIO_OUTPUT@ environment variable.
niPutStr :: MonadIO m => String -> m ()
niPutStr str = liftIO $ withMVar niHandle $ \h -> hPutStr h str >> hFlush h

-- | Non-interleaved version of 'putStrLn'
--
-- See 'niPutStr' for additional discussion.
niPutStrLn :: MonadIO m => String -> m ()
niPutStrLn = niPutStr . (++ "\n")

-- | Non-interleaved version of 'print'
--
-- See 'niPutStr' for additional discussion.
niPrint :: MonadIO m => Show a => a -> m ()
niPrint = niPutStrLn . show

{-------------------------------------------------------------------------------
  Tracing
-------------------------------------------------------------------------------}

-- | Non-interleaved version of 'trace'
--
-- This function can safely be used concurrently with 'niPutStr' and the other
-- functions in this module.
niTrace :: String -> a -> a
niTrace str a = unsafePerformIO $ niPutStrLn str >> return a

-- | Non-interleaved version of 'traceShow'
--
-- See 'niTrace' for additional discussion.
niTraceShow :: Show a  => a -> b -> b
niTraceShow = niTrace . show

-- | Non-interleaved version of 'traceShowId'
--
-- See 'niTrace' for additional discussion.
niTraceShowId :: Show a => a -> a
niTraceShowId a = niTraceShow (show a) a

-- | Non-interleaved version of 'traceM'
--
-- See 'niTrace' for additional discussion.
niTraceM :: Applicative m => String -> m ()
niTraceM str = niTrace str $ pure ()

-- | Non-interleaved version of 'traceShowM'
--
-- See 'niTrace' for additional discussion.
niTraceShowM :: (Applicative m, Show a) => a -> m ()
niTraceShowM = niTraceM . show

{-------------------------------------------------------------------------------
  Uniques
-------------------------------------------------------------------------------}

-- | Unique value
--
-- See 'niGetUnique'.
data NiUnique = NiUnique CallSite Int
  deriving stock (Eq)

instance Show NiUnique where
  show (NiUnique cs i) = "\"" ++ prettyCallSite cs ++ "/" ++ show i ++ "\""

-- | Get a unique value (useful for correlating different log messages)
--
-- Each call to 'niGetUnique' will return a value that is unique relative to
-- where 'niGetUnique' is called from.
--
-- /NOTE/: If you are seeing @<unknown>@ when showing the 'NiUnique', this
-- means that the calling function does not have a 'HasCallStack' annotation:
--
-- > yourFunction :: HasCallStack => IO ()
-- > yourFunction = do
-- >     i <- niGetUnique
-- >     ..
--
-- once you add this annotation, you should see @yourFunction@ instead of
-- @<unknown>@. Similarly, if you have local function definitions, it may
-- be useful to give those 'HasCallStack' constraints of their own:
--
-- > yourFunction :: HasCallStack => IO ()
-- > yourFunction = ..
-- >   where
-- >     someLocalFn :: HasCallStack => IO ()
-- >     someLocalFn = do
-- >         i <- niGetUnique
-- >         ..
--
-- In this example the 'HasCallStack' constraint on 'someLocalFn' means that
-- the 'NiUnique' will show @someLocalFn@ instead of @yourFunction@.
niGetUnique :: (MonadIO m, HasCallStack) => m NiUnique
niGetUnique = withFrozenCallStack $
    liftIO $ atomicModifyIORef niUniques $ \uniques ->
      let
        cs = callSite
        i  = HashMap.findWithDefault 1 callSite uniques
      in
        (HashMap.insert cs (succ i) uniques, NiUnique cs i)

-- | Output with 'NiUnique' prefix
--
-- Example:
--
-- > niPutStrAt [i, j] $ "foo: " ++ E.displayException e
--
-- results in output such as
--
-- > ["exampleFun(./Example/File.hs:100:5)/1","exampleFun2(./Example/File.hs:120:13)/1"]
-- >   foo: ExampleException
-- >   HasCallStack backtrace:
-- >     collectBacktraces, called at (..)
-- >     toExceptionWithBacktrace, called at (..)
-- >     throwIO, called at (..)
niPutStrAt :: MonadIO m => [NiUnique] -> String -> m ()
niPutStrAt is str =
    niPutStrLn $
      case lines str of
        [one] -> show is ++ " " ++ one
        many  -> intercalate "\n" $ show is : map ("  " ++) many

{-------------------------------------------------------------------------------
  Derived functionality
-------------------------------------------------------------------------------}

-- | Print a message before and after an action
--
-- In order to make it easier to correlate the messages before and after the
-- action, we give both a newly created 'Unique' (see 'niGetUnique').
--
-- A common way to invoke 'niBracket' is
--
-- > niBracket (\i -> niPutStrAt [i] "start") (\i -> niPutStrAt [i] . show) $ \i ->
--
-- resulting in output such as
--
-- > ["exampleFun(./Example/File.hs:100:5)/1"] start
-- > ..
-- > ["exampleFun(./Example/File.hs:100:5)/1"] ExitCaseSuccess ()
--
-- When nesting calls to 'niBracket', it can be useful to combine the uniques
-- in order to get better correlation:
--
-- > niBracket (\i -> niPutStrAt [i] "start") (\i -> niPutStrAt [i] . show) $ \i ->
-- >   ..
-- >   niBracket (\j -> niPutStrAt [i, j] "start") (\j -> niPutStrAt [i, j] . show) $ \j ->
-- >     ..
--
-- resulting in output such as
--
-- > ["exampleFun(./Example/File.hs:100:5)/1"] start
-- > ..
-- > ["exampleFun(./Example/File.hs:100:5)/1","exampleFun(./Example/File.hs:120:13)/1"] start
-- > ..
-- > ["exampleFun(./Example/File.hs:100:5)/1","exampleFun(./Example/File.hs:120:13)/1"] ExitCaseSuccess ()
-- > ..
-- > ["exampleFun(./Example/File.hs:100:5)/1"] ExitCaseSuccess ()
--
-- NOTE: We provide an (orphan) 'Functor' instance for 'ExitCase', which can
-- be useful in cases where @a@ is not showable.
niBracket ::
     (MonadIO m, MonadMask m, HasCallStack)
  => (NiUnique -> m ())                  -- ^ Prior to the action
  -> (NiUnique -> ExitCase a -> m ())  -- ^ After
  -> (NiUnique -> m a)
  -> m a
niBracket before after act = withFrozenCallStack $
    fmap (\(a, ()) -> a) $ do
      i <- niGetUnique
      generalBracket
        (before i)
        (\() -> after i)
        (\() -> act i)

-- | See 'niBracket'
deriving stock instance Functor ExitCase

{-------------------------------------------------------------------------------
  Internal: callsites
-------------------------------------------------------------------------------}

data CallSite = CallSite {
      calledFromLoc :: SrcLoc
    , calledFromFn  :: Maybe String
    }
  deriving stock (Eq)

prettyCallSite :: CallSite -> String
prettyCallSite CallSite{calledFromLoc, calledFromFn} = concat [
      case calledFromFn of
         Nothing -> "<unknown>"
         Just fn -> fn
    , "("
    , intercalate ":" [
          srcLocFile calledFromLoc
        , show $ srcLocStartLine calledFromLoc
        , show $ srcLocStartCol calledFromLoc
        ]
    , ")"
    ]

instance Hashable CallSite where
  hashWithSalt salt CallSite{calledFromLoc, calledFromFn} =
      hashWithSalt salt (prettySrcLoc calledFromLoc, calledFromFn)

callSite :: HasCallStack => CallSite
callSite = aux callStack
  where
    aux :: CallStack -> CallSite
    aux cs =
        case getCallStack cs of
          (_, loc) : []          -> CallSite loc Nothing
          (_, loc) : (fn, _) : _ -> CallSite loc (Just fn)
          []                     -> bug "callSite: emptycallstack"

{-------------------------------------------------------------------------------
  Internal: globals
-------------------------------------------------------------------------------}

niHandle :: MVar Handle
{-# NOINLINE niHandle #-}
niHandle = unsafePerformIO $ do
    mOutput <- lookupEnv "NIIO_OUTPUT"
    case mOutput of
      Nothing -> do
        tmpDir  <- getCanonicalTemporaryDirectory
        (fp, h) <- openTempFile tmpDir "niio"
        hPutStrLn stderr $ "niio output to " ++ fp
        newMVar h
      Just fp -> do
        hPutStrLn stderr $ "niio output to " ++ fp
        newMVar =<< openFile fp WriteMode

niUniques :: IORef (HashMap CallSite Int)
{-# NOINLINE niUniques #-}
niUniques = unsafePerformIO $ newIORef HashMap.empty

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

bug :: String -> a
bug str = error . unlines $ [
      str
    , "Please report this as a bug at https://github.com/well-typed/niio"
    ]
