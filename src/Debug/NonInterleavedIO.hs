-- | Functions for non-interleaved output
--
-- Intended for qualifed import.
--
-- > import qualified Debug.NonInterleavedIO as NIIO
--
-- Alternatively, you can import "Debug.NonInterleavedIO.Trace" as a drop-in
-- replacement for "Debug.Trace".
--
-- The functions in this module can all be called concurrently, without
-- resulting in interleaved output: each function call is atomic.
--
-- The first time any of these functions is called, we lookup the @NIIO_OUTPUT@
-- environment variable. If set, we will write to the file specified (if the
-- file already exists, it will be overwritten). If @NIIO_OUTPUT@ is not set, a
-- temporary file will be created in the system temporary directory; typically
-- such a file will be called @/tmp/niio<number>@. The name of this file is
-- written to @stderr@ (this is the /only/ output origiating from functions in
-- this module that is not written to the file).
module Debug.NonInterleavedIO (
    -- * Output functions
    putStr
  , putStrLn
  , print
    -- * Tracing functions
  , trace
  , traceShow
  , traceShowId
  , traceM
  , traceShowM
  ) where

import Prelude hiding (putStr, putStrLn, print)

import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class
import System.Environment
import System.IO.Temp (getCanonicalTemporaryDirectory)
import System.IO.Unsafe

import qualified System.IO as IO

{-------------------------------------------------------------------------------
  Output functions
-------------------------------------------------------------------------------}

-- | Non-interleaved version of 'Prelude.putStr'
putStr :: MonadIO m => String -> m ()
putStr str = liftIO $ withMVar globalHandle $ \h -> do
    IO.hPutStr h str
    IO.hFlush h

-- | Non-interleaved version of 'Prelude.putStrLn'
putStrLn :: MonadIO m => String -> m ()
putStrLn = putStr . (++ "\n")

-- | Non-interleaved version of 'Prelude.print'
print :: MonadIO m => Show a => a -> m ()
print = putStrLn . show

{-------------------------------------------------------------------------------
  Tracing
-------------------------------------------------------------------------------}

-- | Non-interleaved version of 'Debug.Trace.trace'
trace :: String -> a -> a
trace str a = unsafePerformIO $ putStrLn str >> return a

-- | Non-interleaved version of 'Debug.Trace.traceShow'
traceShow :: Show a  => a -> b -> b
traceShow = trace . show

-- | Non-interleaved version of 'Debug.Trace.traceShowId'
traceShowId :: Show a => a -> a
traceShowId a = traceShow (show a) a

-- | Non-interleaved version of 'Debug.Trace.traceM'
traceM :: Applicative m => String -> m ()
traceM str = trace str $ pure ()

-- | Non-interleaved version of 'Debug.Trace.traceShowM'
traceShowM :: (Applicative m, Show a) => a -> m ()
traceShowM = traceM . show

{-------------------------------------------------------------------------------
  Internal: globals
-------------------------------------------------------------------------------}

globalHandle :: MVar IO.Handle
{-# NOINLINE globalHandle #-}
globalHandle = unsafePerformIO $ uninterruptibleMask_ $ do
    mOutput <- lookupEnv "NIIO_OUTPUT"
    (fp, h) <- case mOutput of
                 Nothing -> do
                   tmpDir <- getCanonicalTemporaryDirectory
                   IO.openTempFile tmpDir "niio"
                 Just fp -> do
                   (fp,) <$> IO.openFile fp IO.WriteMode
    IO.hPutStrLn IO.stderr $ "niio output to " ++ fp
    IO.hFlush IO.stderr
    newMVar h
