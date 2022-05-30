module GlobalOptions
  ( module GlobalOptions,
  )
where

import MiniJuvix.Prelude

data GlobalOptions = GlobalOptions
  { _globalNoColors :: Bool,
    _globalShowNameIds :: Bool,
    _globalOnlyErrors :: Bool,
    _globalNoTermination :: Bool,
    _globalInputFiles :: NonEmpty FilePath
  }
  deriving stock (Eq, Show)

makeLenses ''GlobalOptions
