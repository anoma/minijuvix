module MiniJuvix.Pipeline.EntryPoint
  ( module MiniJuvix.Pipeline.EntryPoint,
  )
where

import MiniJuvix.Prelude

newtype Options = Options
  { _optionsNoTermination :: Bool
  }
  deriving stock (Eq, Show)

makeLenses ''Options

-- | The head of _entryModulePaths is assumed to be the Main module
data EntryPoint = EntryPoint
  { _entryPointRoot :: FilePath,
    _entryPointOptions :: Options,
    _entryPointModulePaths :: NonEmpty FilePath
  }
  deriving stock (Eq, Show)

makeLenses ''EntryPoint

mainModulePath :: Lens' EntryPoint FilePath
mainModulePath = entryPointModulePaths . _head
