-- | Drop-in replacement for "Debug.Trace"
--
-- This module just re-exports some functions from "Debug.NonInterleavedIO";
-- since it does not export anything that clashes with "Prelude" it can be
-- imported unqualified.
module Debug.NonInterleavedIO.Trace (
    trace
  , traceShow
  , traceShowId
  , traceM
  , traceShowM
  ) where

import Debug.NonInterleavedIO
