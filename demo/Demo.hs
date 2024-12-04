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
          DemoNiio       ex -> NIIO.demo       ex
          DemoCallsite   ex -> Callsite.demo   ex
          DemoInvocation ex -> Invocation.demo ex
          DemoScope      ex -> Scope.demo      ex
          DemoCallback   ex -> Callback.demo   ex

