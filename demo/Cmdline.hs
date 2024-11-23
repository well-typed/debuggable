module Cmdline (
    Cmdline(..)
  , Demo(..)
  , getCmdline
  ) where

import Options.Applicative

import Demo.Invocation qualified as Invocation
import Demo.Scope      qualified as Scope

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

data Cmdline = Cmdline {
      cmdDemo :: Maybe Demo
    }
  deriving stock (Show)

data Demo =
    DemoNiioWithout
  | DemoNiioUse
  | DemoCallsiteWithout
  | DemoCallsiteUse
  | DemoInvocationWithout
  | DemoInvocationUse Invocation.Example
  | DemoScopeUse Scope.Example
  | DemoCallbackWithout
  | DemoCallbackUse
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
        ( parseWithoutUse
            (pure DemoNiioWithout)
            (pure DemoNiioUse)
        )
        "Demo Debug.NonInterleavedIO"
    , command'
        "callsite"
        ( parseWithoutUse
            (pure DemoCallsiteWithout)
            (pure DemoCallsiteUse)
        )
        "Demo CallSite from Debug.Provenance"
    , command'
        "invocation"
        ( parseWithoutUse
            (pure DemoInvocationWithout)
            (DemoInvocationUse <$> parseInvocationExample)
        )
        "Demo Invocation from Debug.Provenance"
    , command'
        "scope"
        ( DemoScopeUse <$> parseScopeExample )
        "Demo Debug.Provenance.Scope"
    , command'
        "callback"
        ( parseWithoutUse
            (pure DemoCallbackWithout)
            (pure DemoCallbackUse)
        )
        "Demo Debug.Provenance.Callback"
    ]

parseWithoutUse :: Parser a -> Parser a -> Parser a
parseWithoutUse without use = subparser $ mconcat [
      command' "without-debuggable" without "Without debuggable"
    , command' "use-debuggable"     use     "Use debuggable"
    ]

parseInvocationExample :: Parser Invocation.Example
parseInvocationExample = subparser $ mconcat [
      command' "example1" (pure Invocation.Example1) "Example 1"
    , command' "example2" (pure Invocation.Example2) "Example 2"
    ]

parseScopeExample :: Parser Scope.Example
parseScopeExample = subparser $ mconcat [
      command' "example1" (pure Scope.Example1) "Example 1"
    , command' "example2" (pure Scope.Example2) "Example 2"
    , command' "example3" (pure Scope.Example3) "Example 3"
    , command' "example4" (pure Scope.Example4) "Example 4"
    ]

{-------------------------------------------------------------------------------
  Auxiliary optparse-applicative
-------------------------------------------------------------------------------}

command' :: String -> Parser a -> String -> Mod CommandFields a
command' cmd parser desc = command cmd $ info (parser <**> helper) $ mconcat [
      progDesc desc
    ]
