module Cmdline (
    Cmdline(..)
  , Demo(..)
  , getCmdline
  ) where

import Options.Applicative

import Demo.Callback   qualified as Callback
import Demo.Callsite   qualified as Callsite
import Demo.Invocation qualified as Invocation
import Demo.NIIO       qualified as NIIO
import Demo.Scope      qualified as Scope

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

data Cmdline = Cmdline {
      cmdDemo :: Maybe Demo
    }
  deriving stock (Show)

data Demo =
    DemoNiio NIIO.Example
  | DemoCallsite Callsite.Example
  | DemoInvocation Invocation.Example
  | DemoScope Scope.Example
  | DemoCallback Callback.Example
  deriving stock (Show)

getCmdline :: IO Cmdline
getCmdline = execParser opts
  where
    opts :: ParserInfo Cmdline
    opts = info (parseCmdline <**> helper) $ mconcat [
          fullDesc
        , header "Demo of the debuggable package"
        ]

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

parseCmdline :: Parser Cmdline
parseCmdline =
    Cmdline
      <$> optional parseDemo

parseDemo :: Parser Demo
parseDemo = subparser $ mconcat [
      command'
        "niio"
        (DemoNiio <$> parseNiioExample)
        "Demo Debug.NonInterleavedIO"
    , command'
        "callsite"
        (DemoCallsite <$> parseCallsiteExample)
        "Demo CallSite from Debug.Provenance"
    , command'
        "invocation"
         (DemoInvocation <$> parseInvocationExample)
        "Demo Invocation from Debug.Provenance"
    , command'
        "scope"
        (DemoScope <$> parseScopeExample)
        "Demo Debug.Provenance.Scope"
    , command'
        "callback"
        (DemoCallback <$> parseCallbackExample)
        "Demo Debug.Provenance.Callback"
    ]

parseNiioExample :: Parser NIIO.Example
parseNiioExample = subparser $ mconcat [
      command'
        "without-debuggable"
        (pure NIIO.WithoutDebuggable)
        "Without debuggable"
    , command'
        "use-debuggable"
        (pure NIIO.UseDebuggable)
        "Use debuggable"
    ]

parseCallsiteExample :: Parser Callsite.Example
parseCallsiteExample = subparser $ mconcat [
      command'
        "without-debuggable"
        (pure Callsite.WithoutDebuggable)
        "Without debuggable"
    , command'
        "use-debuggable"
        (pure Callsite.UseDebuggable)
        "Use debuggable"
    ]

parseInvocationExample :: Parser Invocation.Example
parseInvocationExample = subparser $ mconcat [
      command'
        "without-debuggable"
        (pure Invocation.WithoutDebuggable)
        "Without debuggable"
    , command'
        "use-debuggable-1"
        (pure $ Invocation.UseDebuggable Invocation.Example1)
        "Use debuggable, example 1"
    , command'
        "use-debuggable-2"
        (pure $ Invocation.UseDebuggable Invocation.Example2)
        "Use debuggable, example 2"
    ]

parseScopeExample :: Parser Scope.Example
parseScopeExample = subparser $ mconcat [
      command'
        "example1"
        (pure Scope.Example1)
        "Use debuggable, example 1"
    , command'
        "example2"
        (pure Scope.Example2)
        "Use debuggable, example 2"
    , command'
        "example3"
        (pure Scope.Example3)
        "Use debuggable, example 3"
    , command'
        "example4"
        (pure Scope.Example4)
        "Use debuggable, example 4"
    ]

parseCallbackExample :: Parser Callback.Example
parseCallbackExample = subparser $ mconcat [
      command'
        "without-debuggable"
        (pure Callback.WithoutDebuggable)
        "Without debuggable"
    , command'
        "use-debuggable"
        (pure Callback.UseDebuggable)
        "Use debuggable"
    , command'
        "use-profiling-1"
        (pure $ Callback.UseProfiling Nothing)
        "Use profiling, example 1"
    , command'
        "use-profiling-2"
        (Callback.UseProfiling . Just <$> argument auto (metavar "INT"))
        "Use profiling, example 2"
    ]

{-------------------------------------------------------------------------------
  Auxiliary optparse-applicative
-------------------------------------------------------------------------------}

command' :: String -> Parser a -> String -> Mod CommandFields a
command' cmd parser desc = command cmd $ info (parser <**> helper) $ mconcat [
      progDesc desc
    ]

