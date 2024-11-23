-- | Utilities for writing debugging messages that include provenance
--
-- Intended for qualified import.
--
-- > import Debug.NonInterleavedIO.Scoped qualified as Scoped
module Debug.NonInterleavedIO.Scoped (
    putStrLn
  ) where

import Prelude hiding (putStrLn)

import Control.Monad.IO.Class
import Data.List (intercalate)

import Debug.NonInterleavedIO qualified as NIIO
import Debug.Provenance.Internal
import Debug.Provenance.Scope

{-------------------------------------------------------------------------------
  Uniques
-------------------------------------------------------------------------------}

-- | Print debug message, showing current scope
putStrLn :: (HasCallStack, MonadIO m) => String -> m ()
putStrLn str = do
    scope <- getScope
    here  <- newInvocationFrom callSite -- the call to 'putStrLn'

    let prettyScope :: String
        prettyScope = concat [
            "["
          , intercalate ", " $ map prettyInvocation (here : scope)
          , "]"
          ]

    NIIO.putStrLn $
      case lines str of
        [one] -> prettyScope ++ " " ++ one
        many  -> intercalate "\n" $ prettyScope : map ("  " ++) many
