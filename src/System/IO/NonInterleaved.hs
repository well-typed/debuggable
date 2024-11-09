{-# OPTIONS_GHC -Wno-orphans #-}

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
    -- * Derived functionality
  , niBracket
  , niBracketLn
  ) where

import Control.Concurrent
import Control.Monad.Catch (MonadMask, ExitCase(..), generalBracket)
import Control.Monad.IO.Class
import System.IO
import System.IO.Temp (getCanonicalTemporaryDirectory)
import System.IO.Unsafe

{-------------------------------------------------------------------------------
  Output functions
-------------------------------------------------------------------------------}

-- | Non-interleaved version of 'putStr'
--
-- Concurrent calls to 'niPutStr' will not result in interleaved output.
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
  Derived functionality
-------------------------------------------------------------------------------}

-- | Print a message before and after an action
--
-- NOTE: We provide an (orphan) 'Functor' instance for 'ExitCase', which can
-- be useful in cases where @a@ is not showable.
niBracket ::
     (MonadIO m, MonadMask m)
  => String                  -- ^ Message to print prior to the action
  -> (ExitCase a -> String)  -- ^ Message to print after
  -> m a -> m a
niBracket before after act = fmap (\(a, ()) -> a) $
    generalBracket
      (niPutStr before)
      (\() -> niPutStr . after)
      (\() -> act)

-- | Like 'niBracket', but adding linebreaks.
--
-- 'niBracketLn' is to 'niBracket' as 'niPutStrLn' is to 'niPutStr'.
niBracketLn ::
     (MonadIO m, MonadMask m)
  => String -> (ExitCase a -> String) -> m a -> m a
niBracketLn before after = niBracket (before ++ "\n") ((++ "\n") . after)

deriving stock instance Functor ExitCase

{-------------------------------------------------------------------------------
  Internal
-------------------------------------------------------------------------------}

niHandle :: MVar Handle
{-# NOINLINE niHandle #-}
niHandle = unsafePerformIO $ do
    tmpDir  <- getCanonicalTemporaryDirectory
    (fp, h) <- openTempFile tmpDir "niio"
    hPutStrLn stderr $ "niio output to " ++ fp
    newMVar h
