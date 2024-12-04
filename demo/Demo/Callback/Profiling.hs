-- | Profiling version of the callback demo
--
-- Requires the code to be built with profiling info.
module Demo.Callback.Profiling (demo) where

import GHC.Stack

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

demo :: Maybe Int -> IO ()
demo Nothing  = f1 (\x -> g1 x)
demo (Just i) = f1 (\x -> g1 (x + i))

{-------------------------------------------------------------------------------
  Demo proper
-------------------------------------------------------------------------------}

f1 :: (Int -> IO ()) -> IO ()
f1 k = do
    cs <- whoCreated k
    putStrLn $ "f1: invoking callback defined at " ++ show (cs)
    f2 k

f2 :: (Int -> IO ()) -> IO ()
f2 k = k 1

g1 :: Int -> IO ()
g1 n = g2 n

g2 :: Int -> IO ()
g2 n = do
    cs <- currentCallStack
    putStrLn $ "n = " ++ show n ++ " at " ++ show cs
