{-# OPTIONS_GHC -Wno-orphans #-}

-- | Utilities for writing debugging messages that include provenance
--
-- Intended for qualified import.
--
-- > import qualified Debug.NonInterleavedIO.Scoped as Scoped
module Debug.NonInterleavedIO.Scoped (
    putStrLn
  ) where

import Prelude hiding (putStrLn)

import Control.Monad.IO.Class
import Data.List (intercalate)
import GHC.Stack

import Debug.Provenance
import Debug.Provenance.Scope

import qualified Debug.NonInterleavedIO as NIIO

{-------------------------------------------------------------------------------
  Uniques
-------------------------------------------------------------------------------}

-- | Print debug message, showing current scope
putStrLn :: (HasCallStack, MonadIO m) => String -> m ()
putStrLn str = withFrozenCallStack $ do
    scope <- getScope
    here  <- newInvocation callSite

    let prettyScope :: String
        prettyScope = intercalate "," $ map prettyInvocation (here : scope)

    NIIO.putStrLn $
      case lines str of
        [one] -> prettyScope ++ " " ++ one
        many  -> intercalate "\n" $ prettyScope : map ("  " ++) many
