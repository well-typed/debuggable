# `niio`: functions for debugging that guarantee non-interleaved output

This package provides analogues of various standard output functions; all of
these functions are safe to use in a concurrent setting, and guarantee that the
output of one function call will not be interleaved with the output of another.
Examples include

```haskell
niPutStr      :: MonadIO m => String -> m ()
niPutStrLn    :: MonadIO m => String -> m ()
niPrint       :: (MonadIO m, Show a) => a -> m ()
niTrace       :: String -> a -> a
niTraceShow   :: Show a => a -> b -> b
niTraceShowId :: Show a => a -> a
niTraceM      :: Applicative m => String -> m ()
niTraceShowM  :: (Applicative m, Show a) => a -> m ()
```

In addition, we provide some support for creating uniques, to be able to
support correlating log messages, and some functionality for working with
these uniques:

```haskell
niGetUnique :: (MonadIO m, HasCallStack) => m NiUnique
niPutStrAt  :: MonadIO m => [NiUnique] -> String -> m ()
```

and

```haskell
niBracket ::
     (MonadIO m, MonadMask m, HasCallStack)
  => (NiUnique -> m ())                -- ^ Prior to the action
  -> (NiUnique -> ExitCase a -> m ())  -- ^ After
  -> (NiUnique -> m a)
  -> m a
```

For example:

```haskell
-- > niBracket (\i -> niPutStrAt [i] "start") (\i -> niPutStrAt [i] . show) $ \i ->
-- >   ..
-- >   niBracket (\j -> niPutStrAt [i, j] "start") (\j -> niPutStrAt [i, j] . show) $ \j ->
-- >     ..
-- >     niPutStrAt [i, j] $ "foo: " ++ E.displayException e
-- >     ..
```

might result in

```
-- > ["exampleFun(./Example/File.hs:100:5)/1"] start
-- > ["exampleFun(./Example/File.hs:100:5)/1","exampleFun(./Example/File.hs:120:13)/1"] start
-- > ["exampleFun(./Example/File.hs:100:5)/1","exampleFun(./Example/File.hs:120:13)/1"]
-- >   foo: ExampleException
-- >   HasCallStack backtrace:
-- >     collectBacktraces, called at (..)
-- >     toExceptionWithBacktrace, called at (..)
-- >     throwIO, called at (..)
-- > ["exampleFun(./Example/File.hs:100:5)/1","exampleFun(./Example/File.hs:120:13)/1"] ExitCaseSuccess ()
-- > ["exampleFun(./Example/File.hs:100:5)/1"] ExitCaseSuccess ()
```

This package is intended for debugging only.
