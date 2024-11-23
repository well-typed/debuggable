module Demo (main) where

import Cmdline

import Demo.Callback   qualified as Callback
import Demo.Callsite   qualified as Callsite
import Demo.Invocation qualified as Invocation
import Demo.NIIO       qualified as NIIO
import Demo.Scope      qualified as Scope

main :: IO ()
main = do
    Cmdline{cmdDemo} <- getCmdline
    case cmdDemo of
      Nothing   -> putStrLn "Please select a demo (see --help)"
      Just demo ->
        case demo of
          DemoNiioWithout       -> NIIO.withoutDebuggable
          DemoNiioUse           -> NIIO.useDebuggable
          DemoCallsiteWithout   -> Callsite.withoutDebuggable
          DemoCallsiteUse       -> Callsite.useDebuggable
          DemoInvocationWithout -> Invocation.withoutDebuggable
          DemoInvocationUse ex  -> Invocation.useDebuggable ex
          DemoScopeUse ex       -> Scope.useDebuggable ex
          DemoCallbackWithout   -> Callback.withoutDebuggable
          DemoCallbackUse       -> Callback.useDebuggable

