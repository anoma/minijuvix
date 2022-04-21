module MiniJuvix.Syntax.MiniHaskell.MiniHaskellResult where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.MonoJuvix.MonoJuvixResult qualified as Mono
import MiniJuvix.Syntax.MiniHaskell.Language

data MiniHaskellResult = MiniHaskellResult
  { _resultMonoJuvix :: Mono.MonoJuvixResult,
    _resultModules :: NonEmpty Module
  }

makeLenses ''MiniHaskellResult
