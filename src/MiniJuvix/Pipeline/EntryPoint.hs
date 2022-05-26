module MiniJuvix.Pipeline.EntryPoint
  ( module MiniJuvix.Pipeline.EntryPoint,
  )
where

import MiniJuvix.Prelude

data GlobalOptions = GlobalOptions
  { _globalNoColors :: Bool,
    _globalShowNameIds :: Bool,
    _globalOnlyErrors :: Bool,
    _globalNoTermination :: Bool
  }
  deriving stock (Eq, Show)

defaultGlobalOptions :: GlobalOptions
defaultGlobalOptions =
  GlobalOptions
    { _globalNoColors = False,
      _globalShowNameIds = False,
      _globalOnlyErrors = False,
      _globalNoTermination = False
    }

-- | The head of _entryModulePaths is assumed to be the Main module
data EntryPoint = EntryPoint
  { _entryPointRoot :: FilePath,
    _entryPointOptions :: GlobalOptions,
    _entryPointModulePaths :: NonEmpty FilePath
  }
  deriving stock (Eq, Show)

makeLenses ''GlobalOptions
makeLenses ''EntryPoint

mainModulePath :: Lens' EntryPoint FilePath
mainModulePath = entryPointModulePaths . _head
