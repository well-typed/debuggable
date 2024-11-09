# `niio`: simple IO functions that guarantee non-interleaved output

This package provides analogues of various standard output functions; all of
these functions are safe to use in a concurrent setting, and guarantee that the
output of one function call will not be interleaved with the output of another.
Examples include

```haskell
niPutStr      :: String -> IO ()
niPutStrLn    :: String -> IO ()
niPrint       :: Show a => a -> IO ()
niTrace       :: String -> a -> a
niTraceShow   :: Show a  => a -> b -> b
niTraceShowId :: Show a => a -> a
niTraceM      :: Applicative m => String -> m ()
niTraceShowM  :: (Applicative m, Show a) => a -> m ()
```

This package is intended for debugging only. 
